mod collector;
mod convert;

pub use collector::Collector;

pub fn is_checkmate_enabled() -> bool {
    #[cfg(debug_assertions)]
    {
        let flag = std::env::var("ROC_CHECKMATE");
        flag.as_deref() == Ok("1")
    }
    #[cfg(not(debug_assertions))]
    {
        false
    }
}

#[macro_export]
macro_rules! debug_checkmate {
    ($opt_collector:expr, $cm:ident => $expr:expr) => {
        #[cfg(debug_assertions)]
        {
            if let Some($cm) = $opt_collector.as_mut() {
                $expr
            }
        }
    };
}

#[macro_export]
macro_rules! with_checkmate {
    ({ on => $on:expr, off => $off:expr, }) => {{
        #[cfg(debug_assertions)]
        {
            $on
        }
        #[cfg(not(debug_assertions))]
        {
            $off
        }
    }};
}
