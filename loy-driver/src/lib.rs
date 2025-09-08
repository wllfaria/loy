use loy_query::QueryEngine;

pub struct GlobalCtx<'ctx> {
    pub query_engine: QueryEngine<'ctx>,
}