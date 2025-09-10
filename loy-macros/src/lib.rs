mod query;

#[proc_macro]
pub fn loy_queries(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    query::loy_queries_inner(input)
}