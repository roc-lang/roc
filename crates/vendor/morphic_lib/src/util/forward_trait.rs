macro_rules! forward_trait {
    (
        $(#[$annot:meta])*
        $t_vis:vis trait $t_name:ident {
            $($methods:tt)*
        }

        $($impls:tt)*
    ) => {
        $(#[$annot])*
        $t_vis trait $t_name { $($methods)* }

        forward_trait_impls!(trait $t_name { $($methods)* } $($impls)*);
    };
}

macro_rules! forward_trait_impls {
    (
        trait $t_name:ident { $($methods:tt)* }
    ) => {
        // Base case: no impls left
    };

    (
        trait $t_name:ident { $($methods:tt)* }

        impl $wrapper:ident => .$field:ident;

        $($impls:tt)*
    ) => {
        impl $t_name for $wrapper {
            forward_trait_impl_body!( { $($methods)* } .$field );
        }

        forward_trait_impls!(trait $t_name { $($methods)* } $($impls)*);
    }
}

macro_rules! forward_trait_impl_body {
    (
        {}
        .$field:ident
    ) => {
        // Base case: no methods left
    };

    (
        {
            $(#[$annot:meta])*
            fn $fn_name:ident(self $(, $arg_name:ident : $arg_ty:ty)* $(,)? ) -> $ret_ty:ty ;
            $($methods:tt)*
        }
        .$field:ident
    ) => {
        fn $fn_name(self, $($arg_name: $arg_ty),*) -> $ret_ty {
            self.$field.$fn_name($($arg_name),*)
        }

        forward_trait_impl_body!({ $($methods)* } .$field);
    };

    (
        {
            $(#[$annot:meta])*
            fn $fn_name:ident(&self $(, $arg_name:ident : $arg_ty:ty)* $(,)? ) -> $ret_ty:ty ;
            $($methods:tt)*
        }
        .$field:ident
    ) => {
        fn $fn_name(&self, $($arg_name: $arg_ty),*) -> $ret_ty {
            self.$field.$fn_name($($arg_name),*)
        }

        forward_trait_impl_body!({ $($methods)* } .$field);
    };

    (
        {
            $(#[$annot:meta])*
            fn $fn_name:ident(&mut self $(, $arg_name:ident : $arg_ty:ty)* $(,)? ) -> $ret_ty:ty ;
            $($methods:tt)*
        }
        .$field:ident
    ) => {
        fn $fn_name(&mut self, $($arg_name: $arg_ty),*) -> $ret_ty {
            self.$field.$fn_name($($arg_name),*)
        }

        forward_trait_impl_body!({ $($methods)* } .$field);
    };
}
