use quote::quote;
use syn::Error;
use syn::spanned::Spanned;

mod keyword {
    syn::custom_keyword!(query);
    syn::custom_keyword!(cache);
}

fn check_attributes(attributes: Vec<syn::Attribute>) -> syn::Result<Vec<syn::Attribute>> {
    let inner = |attr: syn::Attribute| {
        if !attr.path().is_ident("doc") {
            Err(Error::new(
                attr.span(),
                "attributes not supported on queries",
            ))
        } else if !matches!(attr.style, syn::AttrStyle::Outer) {
            Err(Error::new(
                attr.span(),
                "attributes must be outer attributes (`///`), not inner attributes",
            ))
        } else {
            Ok(attr)
        }
    };

    attributes.into_iter().map(inner).collect()
}

pub struct Query {
    doc_comments: Vec<syn::Attribute>,
    name: syn::Ident,
    key: syn::Pat,
    arg: syn::Type,
    result: syn::ReturnType,
    cache: Option<syn::Type>,
}

impl syn::parse::Parse for Query {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let doc_comments = check_attributes(input.call(syn::Attribute::parse_outer)?)?;
        // consume the `query` keyword
        input.parse::<keyword::query>()?;
        let name = input.parse()?;

        let arg_content;
        syn::parenthesized!(arg_content in input);

        let key = syn::Pat::parse_single(&arg_content)?;
        arg_content.parse::<syn::Token![:]>()?;
        let arg: syn::Type = arg_content.parse()?;

        let result: syn::ReturnType = input.parse()?;

        let cache = input.parse::<Option<keyword::cache>>()?;
        let cache = if cache.is_some() { Some(input.parse::<syn::Type>()?) } else { None };

        input.parse::<syn::Token![;]>()?;

        Ok(Self {
            doc_comments,
            name,
            key,
            arg,
            result,
            cache,
        })
    }
}

pub struct Queries(Vec<Query>);

impl syn::parse::Parse for Queries {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut queries = Vec::new();
        while !input.is_empty() {
            queries.push(input.parse()?);
        }
        Ok(Self(queries))
    }
}

pub fn loy_queries_inner(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let queries = syn::parse_macro_input!(input as Queries);
    let mut query_stream = quote! {};

    for query in queries.0 {
        #[rustfmt::skip]
        let Query { doc_comments, name, arg, cache, result, .. } = &query;

        let result_full = &result;
        // if the query doesn't return anything, append the `-> ()` return type explicitly
        let result = match result {
            syn::ReturnType::Default => quote! { -> () },
            _ => quote! { #result_full },
        };

        let cache = match cache {
            Some(cache) => quote! { #cache },
            None => quote! { () },
        };

        // Add the query to the group
        query_stream.extend(quote! {
            #(#doc_comments)*
            fn #name(#arg) #result, #cache,
        });
    }

    proc_macro::TokenStream::from(quote! {
        #[macro_export]
        macro_rules! define_queries {
            ($macro:ident!) => { $macro! { #query_stream } }
        }
    })
}