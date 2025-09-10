#[macro_export]
macro_rules! define_modules {
    (
        $(fn $name:ident($($K:tt)*) -> $V:ty, $cache:ty,)*
    ) => {
        $(pub mod $name {
            use super::super::*;

            pub type Key = $($K)*;
            pub type Value = $V;
            pub type Cache = $cache;
        })*
    };
}

#[macro_export]
macro_rules! define_engine {
    (
        $(
            $(#[$attr:meta])*
            fn $name:ident($($K:tt)*) -> $V:ty, $cache:ty,
        )*
    ) => {
        use $crate::TyCtx;
        use $crate::query::cache::QueryCache;

        #[derive(Debug)]
        pub struct QueryEngine<'ctx> {
            pub providers: QueryProviders,
            pub caches: QueryCaches,
            _marker: std::marker::PhantomData<&'ctx ()>,
        }

        #[derive(Debug, Default)]
        pub struct QueryCaches {
            $(
                pub $name: QueryCache<queries::$name::Key, queries::$name::Cache>,
            )*
        }

        #[derive(Debug)]
        pub struct QueryProviders {
            $(
                pub $name: for<'ctx> fn(
                    TyCtx<'ctx>,
                    queries::$name::Key,
                ) -> queries::$name::Value,
            )*
        }

        impl Default for QueryProviders {
            fn default() -> Self {
                Self {
                    $(
                        $name: |_, key| $crate::query::plumbing::default_query(stringify!($name), &key),
                    )*
                }
            }
        }

        impl<'ctx> Default for QueryEngine<'ctx> {
            fn default() -> Self {
                Self {
                    providers: QueryProviders::default(),
                    caches: QueryCaches::default(),
                    _marker: std::marker::PhantomData,
                }
            }
        }
    }
}

pub(crate) fn default_query(name: &str, key: &dyn std::fmt::Debug) -> ! {
    panic!(
        "`tcx.{name}({key:?})` cannot be called;\n\
        hint: This means, {name} was likely never assigned to a provider function.\n",
    )
}
