use proc_macro2::{Delimiter, TokenTree, TokenStream as TokenStream2, Span};
use quote::quote;

use crate::helpers::{is_ident, is_punct, is_literal_like, validate_type, parse_type_list, consume_type_path};
use crate::output::LispOutput;
use crate::shared::{parse_attributes, parse_generics_params, parse_where_clause, emit_generics, emit_return_type, emit_where_clause, parse_body_items, parse_self_param, parse_visibility};
use crate::expr::eval_lisp_expr;

// ─── Shared fn signature parsing ─────────────────────────────────────────────
// Extracted from parse_fn and parse_impl_body_item to eliminate duplication.
// Parses: attributes, qualifiers, fn keyword, name, generics, params, return type, where clause.

pub(crate) struct FnSignature {
    pub(crate) attrs: TokenStream2,
    pub(crate) quals: TokenStream2,
    pub(crate) name: proc_macro2::Ident,
    pub(crate) generics: Option<TokenStream2>,
    pub(crate) params: Vec<TokenStream2>,
    pub(crate) return_type: Option<TokenStream2>,
    pub(crate) where_clause: Option<TokenStream2>,
    pub(crate) body_start: usize,
}

/// Parse the fn signature portion from tokens, returning None if `fn` keyword is not found.
pub(crate) fn parse_fn_signature(tokens: &[TokenTree]) -> syn::Result<Option<(FnSignature, /* not-a-fn fallback tokens */ Option<TokenStream2>)>> {
    let mut i = 0;

    // 1. Collect attributes
    let attrs_ts = parse_attributes(tokens, &mut i);

    // 2. Collect qualifiers: pub, pub(crate), pub(super), const, unsafe, async, extern "C"
    let mut qualifiers = Vec::new();
    loop {
        if i >= tokens.len() { break; }
        match &tokens[i] {
            TokenTree::Ident(id) if *id == "pub" => {
                qualifiers.push(tokens[i].clone());
                i += 1;
                if i < tokens.len() {
                    if let TokenTree::Group(g) = &tokens[i] {
                        if g.delimiter() == Delimiter::Parenthesis {
                            qualifiers.push(tokens[i].clone());
                            i += 1;
                        }
                    }
                }
            }
            TokenTree::Ident(id) if *id == "const" || *id == "unsafe" || *id == "async" => {
                qualifiers.push(tokens[i].clone());
                i += 1;
            }
            TokenTree::Ident(id) if *id == "extern" => {
                qualifiers.push(tokens[i].clone());
                i += 1;
                if i < tokens.len() && is_literal_like(&tokens[i]) {
                    qualifiers.push(tokens[i].clone());
                    i += 1;
                }
            }
            _ => break,
        }
    }
    let quals_ts: TokenStream2 = qualifiers.into_iter().collect();

    // 3. Skip `fn` keyword — if absent, this isn't a function
    if i < tokens.len() && is_ident(&tokens[i], "fn") {
        i += 1;
    } else {
        return Ok(None);
    }

    // 4. Function name
    let fn_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(tokens[i].span(), "expected function name"));
        }
    } else {
        let span = if i > 0 { tokens[i - 1].span() } else { Span::call_site() };
        return Err(syn::Error::new(span, "expected function name after `fn`"));
    };

    // 5. Check for <...> generic params
    let generics = parse_generics_params(tokens, &mut i);

    // 6. Parse parameter list: a parenthesized group containing (name Type) pairs
    let mut params = Vec::new();
    if i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let param_tokens: Vec<TokenTree> = g.stream().into_iter().collect();
                let mut is_first_param = true;
                for tt in &param_tokens {
                    if let TokenTree::Group(pg) = tt {
                        if pg.delimiter() == Delimiter::Parenthesis {
                            let inner: Vec<TokenTree> = pg.stream().into_iter().collect();
                            if is_first_param {
                                if let Some(self_param) = parse_self_param(&inner) {
                                    params.push(self_param);
                                    is_first_param = false;
                                    continue;
                                }
                            }
                            if inner.len() >= 2 {
                                let param_name = &inner[0];
                                let raw_type: TokenStream2 = inner[1..].iter().cloned().collect();
                                let param_type = validate_type(raw_type);
                                params.push(quote! { #param_name: #param_type });
                            }
                            is_first_param = false;
                        }
                    }
                }
                i += 1;
            }
        }
    }

    // 7. Parse return type
    let mut return_type_tokens = Vec::new();
    // Check for () as unit return type when followed by more tokens
    if i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis && g.stream().is_empty() && i + 1 < tokens.len() {
                return_type_tokens.push(tokens[i].clone());
                i += 1;
            }
        }
    }
    if return_type_tokens.is_empty() {
        while i < tokens.len() {
            if is_ident(&tokens[i], "where") { break; }
            if let TokenTree::Group(g) = &tokens[i] {
                if g.delimiter() == Delimiter::Parenthesis {
                    break;
                }
            }
            if is_literal_like(&tokens[i]) {
                break;
            }
            return_type_tokens.push(tokens[i].clone());
            i += 1;
        }
    }
    let return_type = if return_type_tokens.is_empty() {
        None
    } else {
        let rt: TokenStream2 = return_type_tokens.into_iter().collect();
        Some(validate_type(rt))
    };

    // 8. Check for `where` clause
    let where_clause = parse_where_clause(tokens, &mut i);

    Ok(Some((FnSignature {
        attrs: attrs_ts,
        quals: quals_ts,
        name: fn_name,
        generics,
        params,
        return_type,
        where_clause,
        body_start: i,
    }, None)))
}

/// Parse a lisp-style method/fn definition from tokens inside an impl/trait body.
/// Input: the contents of a (...) group, e.g. `fn foo ((self &Self) (x i32)) RetType (body)`
/// Also handles: `type Name = Type`
/// Returns the Rust code as TokenStream2.
pub(crate) fn parse_impl_body_item(tokens: &[TokenTree]) -> syn::Result<LispOutput> {
    if tokens.is_empty() {
        return Ok(LispOutput::Tokens(quote! {}));
    }

    // Check for `type Name = Type`
    if is_ident(&tokens[0], "type") {
        let rest: TokenStream2 = tokens[1..].iter().cloned().collect();
        return Ok(LispOutput::Tokens(quote! { type #rest; }));
    }

    // Use shared fn signature parser
    let sig = match parse_fn_signature(tokens)? {
        Some((sig, _)) => sig,
        None => {
            // Not a fn definition — evaluate directly
            return Ok(eval_lisp_expr(tokens));
        }
    };

    // Parse body: remaining tokens are parenthesized groups or bare expressions
    let body_items = parse_body_items(tokens, sig.body_start);
    let has_body = sig.body_start < tokens.len();

    let gen = emit_generics(&sig.generics);
    let ret = emit_return_type(&sig.return_type);
    let where_cl = emit_where_clause(&sig.where_clause);
    let attrs_ts = &sig.attrs;
    let quals_ts = &sig.quals;
    let fn_name = &sig.name;
    let params = &sig.params;

    if has_body {
        Ok(LispOutput::Tokens(quote! {
            #attrs_ts
            #quals_ts fn #fn_name #gen (#(#params),*) #ret #where_cl {
                #(#body_items);*
            }
        }))
    } else {
        Ok(LispOutput::Tokens(quote! {
            #attrs_ts
            #quals_ts fn #fn_name #gen (#(#params),*) #ret #where_cl;
        }))
    }
}

pub(crate) fn parse_assign_op(tokens: &[TokenTree]) -> (Vec<TokenTree>, &[TokenTree]) {
    // Try to parse compound operators like +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=
    // or simple =
    let mut i = 0;
    let mut op = Vec::new();

    if i >= tokens.len() {
        return (op, tokens);
    }

    match &tokens[i] {
        TokenTree::Punct(p) => {
            let ch = p.as_char();
            match ch {
                '=' => {
                    op.push(tokens[i].clone());
                    (op, &tokens[i + 1..])
                }
                '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' => {
                    op.push(tokens[i].clone());
                    i += 1;
                    if i < tokens.len() {
                        if let TokenTree::Punct(p2) = &tokens[i] {
                            if p2.as_char() == '=' {
                                op.push(tokens[i].clone());
                                return (op, &tokens[i + 1..]);
                            }
                        }
                    }
                    // Not a compound op, shouldn't happen
                    (vec![], tokens)
                }
                '<' => {
                    // Could be <<=
                    op.push(tokens[i].clone());
                    i += 1;
                    if i < tokens.len() {
                        if let TokenTree::Punct(p2) = &tokens[i] {
                            if p2.as_char() == '<' {
                                op.push(tokens[i].clone());
                                i += 1;
                                if i < tokens.len() {
                                    if let TokenTree::Punct(p3) = &tokens[i] {
                                        if p3.as_char() == '=' {
                                            op.push(tokens[i].clone());
                                            return (op, &tokens[i + 1..]);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    (vec![], tokens)
                }
                '>' => {
                    // Could be >>=
                    op.push(tokens[i].clone());
                    i += 1;
                    if i < tokens.len() {
                        if let TokenTree::Punct(p2) = &tokens[i] {
                            if p2.as_char() == '>' {
                                op.push(tokens[i].clone());
                                i += 1;
                                if i < tokens.len() {
                                    if let TokenTree::Punct(p3) = &tokens[i] {
                                        if p3.as_char() == '=' {
                                            op.push(tokens[i].clone());
                                            return (op, &tokens[i + 1..]);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    (vec![], tokens)
                }
                _ => (vec![], tokens),
            }
        }
        _ => (vec![], tokens),
    }
}

pub(crate) fn parse_impl(tokens: &[TokenTree]) -> syn::Result<syn::Item> {
    let mut i = 0;

    // 1. Check for <...> generic params
    let generics = parse_generics_params(tokens, &mut i);

    // 2. Consume the first type/trait path
    let (first_path, rest) = consume_type_path(&tokens[i..]);
    if first_path.is_empty() {
        let span = tokens.get(i).map_or(Span::call_site(), |t| t.span());
        return Err(syn::Error::new(span, "lisp_impl!: expected type or trait name"));
    }
    let first_path_ts: TokenStream2 = first_path.into_iter().collect();
    i = tokens.len() - rest.len();

    // 3. Check for `for` keyword
    let (trait_path, impl_type) = if i < tokens.len() && is_ident(&tokens[i], "for") {
        i += 1; // skip `for`
        let (type_path, rest2) = consume_type_path(&tokens[i..]);
        if type_path.is_empty() {
            let span = tokens.get(i).map_or(tokens[i - 1].span(), |t| t.span());
            return Err(syn::Error::new(span, "lisp_impl!: expected type after 'for'"));
        }
        let type_path_ts: TokenStream2 = type_path.into_iter().collect();
        i = tokens.len() - rest2.len();
        (Some(first_path_ts), type_path_ts)
    } else {
        (None, first_path_ts)
    };

    // 4. Check for `where` clause
    let where_clause = parse_where_clause(tokens, &mut i);

    // 5. Remaining tokens should be parenthesized groups containing method/type definitions
    let mut body_items = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                let item = parse_impl_body_item(&inner)?;
                body_items.push(item);
            }
        }
        i += 1;
    }

    // Build the impl block
    let gen = emit_generics(&generics);
    let where_cl = emit_where_clause(&where_clause);
    let trait_for = match &trait_path {
        Some(t) => quote! { #t for },
        None => quote! {},
    };

    Ok(syn::Item::Verbatim(quote! {
        impl #gen #trait_for #impl_type #where_cl {
            #(#body_items)*
        }
    }))
}

pub(crate) fn parse_trait(tokens: &[TokenTree]) -> syn::Result<syn::Item> {
    let mut i = 0;

    // 1. Check for visibility (pub, pub(crate), etc.)
    let (vis_ts, vis_consumed) = parse_visibility(&tokens[i..]);
    i += vis_consumed;

    // 2. Consume trait name
    let trait_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(tokens[i].span(), "lisp_trait!: expected trait name"));
        }
    } else {
        let span = if i > 0 { tokens[i - 1].span() } else { Span::call_site() };
        return Err(syn::Error::new(span, "lisp_trait!: expected trait name"));
    };

    // 3. Check for <...> generic params
    let generics = parse_generics_params(tokens, &mut i);

    // 4. Check for `:` followed by supertraits
    let mut supertraits = None;
    if i < tokens.len() && is_punct(&tokens[i], ':') {
        i += 1; // skip `:`
        let mut supertrait_tokens = Vec::new();
        // Consume tokens until we hit a parenthesized group or `where`
        while i < tokens.len() {
            if let TokenTree::Group(g) = &tokens[i] {
                if g.delimiter() == Delimiter::Parenthesis {
                    break;
                }
            }
            if is_ident(&tokens[i], "where") {
                break;
            }
            supertrait_tokens.push(tokens[i].clone());
            i += 1;
        }
        if !supertrait_tokens.is_empty() {
            let st_ts: TokenStream2 = supertrait_tokens.into_iter().collect();
            supertraits = Some(st_ts);
        }
    }

    // 5. Check for `where` clause
    let where_clause = parse_where_clause(tokens, &mut i);

    // 6. Remaining tokens are method/type declarations in (...) groups
    let mut body_items = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                let item = parse_impl_body_item(&inner)?;
                body_items.push(item);
            }
        }
        i += 1;
    }

    let gen = emit_generics(&generics);
    let super_cl = match &supertraits {
        Some(s) => quote! { : #s },
        None => quote! {},
    };
    let where_cl = emit_where_clause(&where_clause);

    Ok(syn::Item::Verbatim(quote! {
        #vis_ts trait #trait_name #gen #super_cl #where_cl {
            #(#body_items)*
        }
    }))
}

pub(crate) fn parse_enum(tokens: &[TokenTree]) -> syn::Result<syn::Item> {
    let mut i = 0;

    // 1. Check for attributes and pub
    let attrs_ts = parse_attributes(tokens, &mut i);

    let (vis_ts, vis_consumed) = parse_visibility(&tokens[i..]);
    i += vis_consumed;

    // Skip `enum` keyword if present
    if i < tokens.len() && is_ident(&tokens[i], "enum") {
        i += 1;
    }

    // 2. Consume enum name
    let enum_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(tokens[i].span(), "lisp_enum!: expected enum name"));
        }
    } else {
        let span = if i > 0 { tokens[i - 1].span() } else { Span::call_site() };
        return Err(syn::Error::new(span, "lisp_enum!: expected enum name"));
    };

    // 3. Check for <...> generic params
    let generics = parse_generics_params(tokens, &mut i);

    // 4. Parse variants - each is a (...) group
    let mut variants = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let variant_ts = parse_enum_variant(g.stream())?;
                variants.push(variant_ts);
            }
        }
        i += 1;
    }

    let gen = emit_generics(&generics);

    Ok(syn::Item::Verbatim(quote! {
        #attrs_ts
        #vis_ts enum #enum_name #gen {
            #(#variants),*
        }
    }))
}

pub(crate) fn parse_enum_variant(stream: TokenStream2) -> syn::Result<TokenStream2> {
    let tokens: Vec<TokenTree> = stream.into_iter().collect();
    if tokens.is_empty() {
        return Err(syn::Error::new(Span::call_site(), "empty enum variant"));
    }

    // First token should be the variant name
    let variant_name = if let TokenTree::Ident(id) = &tokens[0] {
        id.clone()
    } else {
        return Err(syn::Error::new(tokens[0].span(), "expected variant name"));
    };

    if tokens.len() == 1 {
        // Unit variant
        return Ok(quote! { #variant_name });
    }

    // Check if the next token is a parenthesized group containing (field Type) pairs -> struct variant
    if tokens.len() == 2 {
        if let TokenTree::Group(g) = &tokens[1] {
            if g.delimiter() == Delimiter::Parenthesis {
                // Check if contents look like (field Type) pairs
                let inner_tokens: Vec<TokenTree> = g.stream().into_iter().collect();
                if !inner_tokens.is_empty() {
                    if let TokenTree::Group(inner_g) = &inner_tokens[0] {
                        if inner_g.delimiter() == Delimiter::Parenthesis {
                            // It's a struct variant with (field Type) pairs
                            let fields = parse_struct_variant_fields(g.stream())?;
                            return Ok(quote! { #variant_name { #fields } });
                        }
                    }
                }
            }
        }
    }

    // Otherwise it's a tuple variant - remaining tokens are types
    let type_tokens: Vec<&TokenTree> = tokens[1..].iter().collect();
    let types_ts: TokenStream2 = type_tokens.into_iter().cloned().collect();
    Ok(quote! { #variant_name(#types_ts) })
}

pub(crate) fn parse_struct_variant_fields(stream: TokenStream2) -> syn::Result<TokenStream2> {
    let tokens: Vec<TokenTree> = stream.into_iter().collect();
    let mut fields = Vec::new();

    for tt in &tokens {
        if let TokenTree::Group(g) = tt {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                if inner.len() >= 2 {
                    if let TokenTree::Ident(field_name) = &inner[0] {
                        let raw_type: TokenStream2 = inner[1..].iter().cloned().collect();
                        let field_type = validate_type(raw_type);
                        fields.push(quote! { #field_name: #field_type });
                    }
                }
            }
        }
    }

    let result = if fields.len() == 1 {
        let f = &fields[0];
        quote! { #f }
    } else {
        quote! { #(#fields),* }
    };
    Ok(result)
}

pub(crate) fn parse_struct(tokens: &[TokenTree]) -> syn::Result<syn::Item> {
    let mut i = 0;

    // 1. Collect attributes
    let attrs_ts = parse_attributes(tokens, &mut i);

    // 2. Check for visibility
    let (vis_ts, vis_consumed) = parse_visibility(&tokens[i..]);
    i += vis_consumed;

    // Skip `struct` keyword
    if i < tokens.len() && is_ident(&tokens[i], "struct") {
        i += 1;
    }

    // 3. Consume struct name
    let struct_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(tokens[i].span(), "lisp_struct!: expected struct name"));
        }
    } else {
        let span = if i > 0 { tokens[i - 1].span() } else { Span::call_site() };
        return Err(syn::Error::new(span, "lisp_struct!: expected struct name"));
    };

    // 4. Consume <...> generic params
    let generics = parse_generics_params(tokens, &mut i);

    // 5. Check for where clause
    let where_clause = parse_where_clause(tokens, &mut i);

    let gen = emit_generics(&generics);
    let where_cl = emit_where_clause(&where_clause);

    // 6. Parse fields — collect all remaining parenthesized groups as field groups.
    // Supports: named fields ((f T) ...), pub fields (pub (f T) ...), mixed (pub ...)(...),
    //           tuple structs (T1 T2 ...), and unit structs.
    let mut field_groups = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                field_groups.push(g.clone());
            }
        }
        i += 1;
    }

    if field_groups.is_empty() {
        // Unit struct
        return Ok(syn::Item::Verbatim(quote! { #attrs_ts #vis_ts struct #struct_name #gen #where_cl; }));
    }

    // Determine if named fields or tuple struct by inspecting first group contents
    let first_inner: Vec<TokenTree> = field_groups[0].stream().into_iter().collect();
    if first_inner.is_empty() {
        return Ok(syn::Item::Verbatim(quote! { #attrs_ts #vis_ts struct #struct_name #gen #where_cl; }));
    }

    // Check if this is a named-field struct: first meaningful element is a Group (field definition)
    let is_named = {
        let check_idx = if is_ident(&first_inner[0], "pub") { 1 } else { 0 };
        check_idx < first_inner.len() && matches!(&first_inner[check_idx], TokenTree::Group(_))
    };

    if is_named {
        // Named struct — process all field groups (supports mixed pub/private via multiple groups)
        let mut all_fields = Vec::new();
        for group in &field_groups {
            let inner: Vec<TokenTree> = group.stream().into_iter().collect();
            let mut field_start = 0;
            let mut is_pub = false;
            if !inner.is_empty() && is_ident(&inner[0], "pub") {
                is_pub = true;
                field_start = 1;
            }
            for tt in &inner[field_start..] {
                if let TokenTree::Group(fg) = tt {
                    if fg.delimiter() == Delimiter::Parenthesis {
                        let field_tokens: Vec<TokenTree> = fg.stream().into_iter().collect();
                        if field_tokens.len() >= 2 {
                            let field_name = &field_tokens[0];
                            let raw_type: TokenStream2 = field_tokens[1..].iter().cloned().collect();
                            let field_type = validate_type(raw_type);
                            if is_pub {
                                all_fields.push(quote! { pub #field_name: #field_type });
                            } else {
                                all_fields.push(quote! { #field_name: #field_type });
                            }
                        }
                    }
                }
            }
        }
        Ok(syn::Item::Verbatim(quote! {
            #attrs_ts
            #vis_ts struct #struct_name #gen #where_cl {
                #(#all_fields),*
            }
        }))
    } else {
        // Tuple struct — first group contains space-separated types
        let types_stream: TokenStream2 = first_inner.into_iter().collect();
        let types = parse_type_list(types_stream)
            .map_err(|e| syn::Error::new(e.span(), format!("lisp_struct! tuple fields: {}", e)))?;
        Ok(syn::Item::Verbatim(quote! {
            #attrs_ts
            #vis_ts struct #struct_name #gen #where_cl ( #(#types),* );
        }))
    }
}

pub(crate) fn parse_fn(tokens: &[TokenTree]) -> syn::Result<syn::Item> {
    // Use shared fn signature parser
    let sig = match parse_fn_signature(tokens)? {
        Some((sig, _)) => sig,
        None => {
            let span = tokens.first().map_or(Span::call_site(), |t| t.span());
            return Err(syn::Error::new(span, "lisp_fn!: expected fn keyword"));
        }
    };

    // Body: remaining parenthesized groups evaluated via eval_lisp_expr
    // (consistent with parse_impl_body_item, eliminating the ::lisp::lisp! callback chain)
    let body_items = parse_body_items(tokens, sig.body_start);

    let gen = emit_generics(&sig.generics);
    let ret = emit_return_type(&sig.return_type);
    let where_cl = emit_where_clause(&sig.where_clause);
    let attrs_ts = &sig.attrs;
    let quals_ts = &sig.quals;
    let fn_name = &sig.name;
    let params = &sig.params;

    Ok(syn::Item::Verbatim(quote! {
        #attrs_ts
        #quals_ts fn #fn_name #gen (#(#params),*) #ret #where_cl {
            #(#body_items);*
        }
    }))
}
