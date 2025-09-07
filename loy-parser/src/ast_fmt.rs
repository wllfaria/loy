use crate::ast::*;

pub trait AstFmt {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display;
}

pub struct AstDisplay<'a> {
    pub ast: &'a crate::ast::Ast,
    pub source: &'a str,
}

impl<'a> std::fmt::Display for AstDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "<Root>")?;

        for (idx, statement) in self.ast.statements.iter().enumerate() {
            let is_last = idx == self.ast.statements.len() - 1;
            statement.fmt_ast(f, self.source, "", is_last)?;
        }

        Ok(())
    }
}

impl AstFmt for AstNodeTypeDef {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        write!(f, "{prefix}{branch}")?;

        let name = &source.as_ref()[self.name.position.into_range()];
        writeln!(f, "Typedef {} < {} >", self.kind, name)?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        if !self.generics.is_empty() {
            writeln!(f, "{child_prefix}├─ with generics:")?;

            for (idx, generic) in self.generics.iter().enumerate() {
                let is_last = idx == self.generics.len() - 1;
                let child_prefix = format!("{child_prefix}│  ");
                generic.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
            }
        }

        let kind = match self.kind {
            TypeDeclKind::Enum => "variants",
            TypeDeclKind::Struct => "fields",
            TypeDeclKind::Interface => "contract",
        };
        writeln!(f, "{child_prefix}└─ with {kind}:")?;
        let child_prefix = format!("{child_prefix}   ");
        self.value
            .fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for AstNodeStruct {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        _: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        for (idx, field) in self.fields.iter().enumerate() {
            let is_last = idx == self.fields.len() - 1;
            field.fmt_ast(f, source.as_ref(), prefix.as_ref(), is_last)?;
        }

        Ok(())
    }
}

impl AstFmt for AstNodeEnum {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        _: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        for (idx, variant) in self.variants.iter().enumerate() {
            let is_last = idx == self.variants.len() - 1;
            variant.fmt_ast(f, source.as_ref(), prefix.as_ref(), is_last)?;
        }

        Ok(())
    }
}

impl AstFmt for AstNodeEnumVariant {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(
            f,
            "{prefix}{branch}< {} >",
            &source.as_ref()[self.name.position.into_range()]
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");
        if let Some(data) = self.data.as_ref() {
            data.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
        }

        Ok(())
    }
}

impl AstFmt for AstNodeFunSignature {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(
            f,
            "{prefix}{branch}Function < {} >",
            &source.as_ref()[self.name.position.into_range()]
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");
        let branch = if !self.generics.is_empty() { "├─ " } else { "└─ " };
        writeln!(f, "{child_prefix}{branch}returning:")?;

        match self.return_ty.as_ref() {
            Some(return_ty) => {
                let branch = if !self.generics.is_empty() { "│  " } else { "   " };
                let child_prefix = format!("{child_prefix}{branch}");
                return_ty.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;
            }
            None => {
                let branch = if !self.generics.is_empty() { "│  " } else { "   " };
                let child_prefix = format!("{child_prefix}{branch}");
                writeln!(f, "{child_prefix}└─ void")?;
            }
        };

        if !self.generics.is_empty() {
            writeln!(f, "{child_prefix}├─ with generics:")?;
            for (idx, generic) in self.generics.iter().enumerate() {
                let branch = if self.args.is_empty() { "   " } else { "│  " };
                let is_last = idx == self.generics.len() - 1;
                let child_prefix = format!("{child_prefix}{branch}");
                generic.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
            }
        }

        if !self.args.is_empty() {
            writeln!(f, "{child_prefix}└─ with args:")?;
            for (idx, arg) in self.args.iter().enumerate() {
                let is_last = idx == self.args.len() - 1;
                let child_branch = if is_last { "   " } else { "│  " };
                let child_prefix = format!("{child_prefix}{child_branch}");
                arg.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
            }
        }

        Ok(())
    }
}

impl AstFmt for AstNodeInterface {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        _: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        for (idx, function) in self.functions.iter().enumerate() {
            let is_last = idx == self.functions.len() - 1;
            function.fmt_ast(f, source.as_ref(), prefix.as_ref(), is_last)?;
        }

        Ok(())
    }
}

impl AstFmt for AstNodeStructField {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(
            f,
            "{prefix}{branch}< {} > with type:",
            &source.as_ref()[self.name.position.into_range()]
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");
        self.ty
            .fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for PrimitiveTypeKind {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        write!(f, "{prefix}{branch}")?;

        match self {
            PrimitiveTypeKind::Bool(val) => writeln!(f, "< {val} >"),
            PrimitiveTypeKind::Unsigned(bit_size) => writeln!(f, "< u{bit_size} >"),
            PrimitiveTypeKind::Integer(bit_size) => writeln!(f, "< i{bit_size} >"),
        }
    }
}

impl AstFmt for AstNodeNamedType {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        write!(f, "{prefix}{branch}")?;

        write!(
            f,
            "< {} >",
            &source.as_ref()[self.name.position.into_range()]
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        if !self.generics.is_empty() {
            writeln!(f, " with generics:")?;

            for (idx, generic) in self.generics.iter().enumerate() {
                let is_last = idx == self.generics.len() - 1;
                generic.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
            }
        } else {
            writeln!(f)?;
        }

        Ok(())
    }
}

impl AstFmt for AstNodeTupleType {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        _: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        if !self.types.is_empty() {
            for (idx, ty) in self.types.iter().enumerate() {
                let is_last = idx == self.types.len() - 1;
                ty.fmt_ast(f, source.as_ref(), prefix.as_ref(), is_last)?;
            }
        }

        Ok(())
    }
}

impl AstFmt for AstNodeTypeAnnotation {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        match &self.kind {
            AstNodeTypeKind::Primitive(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNodeTypeKind::Named(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNodeTypeKind::Tuple(node) => node.fmt_ast(f, source, prefix, is_last),
        }
    }
}

impl AstFmt for AstNodeGenericDecl {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        self.ty.fmt_ast(f, source, prefix, is_last)
    }
}

impl AstFmt for AstNodeFun {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        let name = &source.as_ref()[self.name.position.into_range()];
        writeln!(f, "{prefix}{branch}Function < {name} >")?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");
        writeln!(f, "{child_prefix}├─ returning:")?;

        match self.return_ty.as_ref() {
            Some(return_ty) => {
                let branch = if !self.generics.is_empty() { "│  " } else { "   " };
                let child_prefix = format!("{child_prefix}{branch}");
                return_ty.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;
            }
            None => {
                let child_prefix = format!("{child_prefix}│  ");
                writeln!(f, "{child_prefix}└─ void")?;
            }
        };

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        if !self.generics.is_empty() {
            writeln!(f, "{child_prefix}├─ with generics:")?;

            for (idx, generic) in self.generics.iter().enumerate() {
                let is_last = idx == self.generics.len() - 1;
                let child_prefix = format!("{child_prefix}│  ");
                generic.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
            }
        }

        if !self.args.is_empty() {
            writeln!(f, "{child_prefix}├─ with args:")?;
            for (idx, arg) in self.args.iter().enumerate() {
                let is_last = idx == self.args.len() - 1;
                let child_prefix = format!("{child_prefix}│  ");
                arg.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
            }
        }

        writeln!(f, "{child_prefix}└─ with body:")?;
        let child_prefix = format!("{child_prefix}   ");
        self.body
            .fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for AstNodeFunArg {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(
            f,
            "{prefix}{branch}< {} > with type:",
            &source.as_ref()[self.name.position.into_range()]
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");
        self.ty
            .fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for AstNodeImportMethod {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        _: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        write!(
            f,
            "{prefix}< {} >",
            &source.as_ref()[self.name.position.into_range()]
        )?;

        if let Some(alias) = self.alias.as_ref() {
            writeln!(
                f,
                " as alias < {} >",
                &source.as_ref()[alias.position.into_range()]
            )?;
        } else {
            writeln!(f)?;
        }

        Ok(())
    }
}

impl AstFmt for AstNodeImport {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        write!(f, "{prefix}{branch}")?;

        writeln!(
            f,
            "Import <\"{}\">",
            &source.as_ref()[self.path.position.into_range()]
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{child_branch}{prefix}");

        if let Some(alias) = self.alias.as_ref() {
            let branch = if self.methods.is_some() { "├─ " } else { "└─ " };
            writeln!(f, "{child_prefix}{branch}with alias:")?;

            let child_branch = if self.methods.is_some() { "│  " } else { "   " };
            let child_prefix = format!("{child_prefix}{child_branch}");

            writeln!(
                f,
                "{child_prefix}└─ {}",
                &source.as_ref()[alias.name.position.into_range()]
            )?;
        }

        if let Some(methods) = self.methods.as_ref() {
            let branch = format!("{child_prefix}└─ ");
            let label = if methods.methods.len() > 1 { "with methods" } else { "with method" };
            writeln!(f, "{branch}{label}:")?;

            for (idx, method) in methods.methods.iter().enumerate() {
                let is_last = idx == methods.methods.len() - 1;
                let child_branch = if is_last { "└─ " } else { "├─ " };
                let child_prefix = format!("{child_prefix}   {child_branch}");
                method.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
            }
        }

        Ok(())
    }
}

impl AstFmt for AstNode {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        match self {
            AstNode::TypeDef(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNode::Struct(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNode::Enum(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNode::Interface(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNode::Function(node) => node.fmt_ast(f, source, prefix, is_last),
            AstNode::Import(node) => node.fmt_ast(f, source, prefix, is_last),
        }
    }
}

impl AstFmt for BlockExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        _: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        writeln!(f, "{prefix}└─ block:")?;
        let prefix = format!("{prefix}   ");

        if self.exprs.is_empty() {
            write!(f, "{prefix}└─ <empty>")?;
            return Ok(());
        }

        for (idx, expr) in self.exprs.iter().enumerate() {
            let is_last = idx == self.exprs.len() - 1;
            expr.fmt_ast(f, source.as_ref(), prefix.as_ref(), is_last)?;
        }

        Ok(())
    }
}

impl AstFmt for BindingExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };

        let mutability = match self.mutability {
            BindingMutability::Immutable => "Constant",
            BindingMutability::Mutable => "Variable",
        };
        let name = &source.as_ref()[self.name.position.into_range()];
        writeln!(f, "{prefix}{branch}{mutability} < {name} >")?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        if let Some(ty) = self.ty.as_ref() {
            writeln!(f, "{child_prefix}├─ with type:")?;
            let child_prefix = format!("{child_prefix}│  ");
            ty.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;
        }

        writeln!(f, "{child_prefix}└─ with value:")?;
        let child_prefix = format!("{child_prefix}   ");
        self.value
            .fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for NumberExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        write!(f, "{prefix}{branch}")?;

        match self.kind {
            NumberKindExpr::Unsigned(_) => writeln!(f, "uint: {}", self.kind),
            NumberKindExpr::Signed(_) => writeln!(f, "int: {}", self.kind),
            NumberKindExpr::Float(_) => writeln!(f, "float: {}", self.kind),
        }
    }
}

impl AstFmt for BoolExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}Bool < {} >", self.value)
    }
}

impl AstFmt for StringExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        let value = &source.as_ref()[self.position.into_range()];
        writeln!(f, "{prefix}{branch}String \"{value}\"")
    }
}

impl AstFmt for ArrayExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(
            f,
            "{prefix}{branch}Array({}) with values:",
            self.elements.len()
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        if self.elements.is_empty() {
            writeln!(f, "{child_prefix}└─ <empty>")?;
            return Ok(());
        }

        for (idx, element) in self.elements.iter().enumerate() {
            let is_last = idx == self.elements.len() - 1;
            element.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
        }

        Ok(())
    }
}

impl AstFmt for TupleExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(
            f,
            "{prefix}{branch}Tuple({}) with values:",
            self.elements.len()
        )?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        if self.elements.is_empty() {
            writeln!(f, "{child_prefix}└─ <empty>")?;
            return Ok(());
        }

        for (idx, element) in self.elements.iter().enumerate() {
            let is_last = idx == self.elements.len() - 1;
            element.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
        }

        Ok(())
    }
}

impl AstFmt for BinaryExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}BinaryOp < {} >", self.op)?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        writeln!(f, "{child_prefix}├─ left:")?;
        let left_prefix = format!("{child_prefix}│  ");
        self.lhs
            .fmt_ast(f, source.as_ref(), left_prefix.as_ref(), true)?;

        writeln!(f, "{child_prefix}└─ right:")?;
        let right_prefix = format!("{child_prefix}   ");
        self.rhs
            .fmt_ast(f, source.as_ref(), right_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for UnaryExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}UnaryOp < {} >", self.op)?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        writeln!(f, "{child_prefix}└─ operand:")?;
        let operand_prefix = format!("{child_prefix}   ");
        self.operand
            .fmt_ast(f, source.as_ref(), operand_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for FunctionCallExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}FunctionCall")?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        let branch = if self.args.is_empty() { "└─ " } else { "├─ " };
        writeln!(f, "{child_prefix}{branch}callee:")?;

        let callee_branch = if self.args.is_empty() { "   " } else { "│  " };
        let callee_prefix = format!("{child_prefix}{callee_branch}");
        self.callee
            .fmt_ast(f, source.as_ref(), callee_prefix.as_ref(), true)?;

        if !self.args.is_empty() {
            writeln!(f, "{child_prefix}└─ with args:")?;
            let args_prefix = format!("{child_prefix}   ");

            for (idx, arg) in self.args.iter().enumerate() {
                let is_last = idx == self.args.len() - 1;
                arg.fmt_ast(f, source.as_ref(), args_prefix.as_ref(), is_last)?;
            }
        }

        Ok(())
    }
}

impl AstFmt for ArrayAccessExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}ArrayAccess")?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        writeln!(f, "{child_prefix}├─ array:")?;
        let array_prefix = format!("{child_prefix}│  ");
        self.array
            .fmt_ast(f, source.as_ref(), array_prefix.as_ref(), true)?;

        writeln!(f, "{child_prefix}└─ index:")?;
        let index_prefix = format!("{child_prefix}   ");
        self.index
            .fmt_ast(f, source.as_ref(), index_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for MemberAccessExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        let member_name = &source.as_ref()[self.member.position.into_range()];
        writeln!(f, "{prefix}{branch}MemberAccess < {member_name} >")?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        writeln!(f, "{child_prefix}└─ object:")?;
        let object_prefix = format!("{child_prefix}   ");
        self.object
            .fmt_ast(f, source.as_ref(), object_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for IfExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}If")?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        writeln!(f, "{child_prefix}├─ condition:")?;
        let condition_prefix = format!("{child_prefix}│  ");
        self.condition
            .fmt_ast(f, source.as_ref(), condition_prefix.as_ref(), true)?;

        let branch = if self.falsy.is_some() { "├─ " } else { "└─ " };
        writeln!(f, "{child_prefix}{branch}then:")?;

        let then_branch = if self.falsy.is_some() { "│  " } else { "   " };
        let then_prefix = format!("{child_prefix}{then_branch}");
        self.truthy
            .fmt_ast(f, source.as_ref(), then_prefix.as_ref(), true)?;

        if let Some(falsy) = self.falsy.as_ref() {
            writeln!(f, "{child_prefix}└─ else:")?;
            let else_prefix = format!("{child_prefix}   ");
            falsy.fmt_ast(f, source.as_ref(), else_prefix.as_ref(), true)?;
        }

        Ok(())
    }
}

impl AstFmt for SemiColonExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}SemiColon < ; >")
    }
}

impl AstFmt for WhileExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}While")?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        writeln!(f, "{child_prefix}├─ condition:")?;
        let condition_prefix = format!("{child_prefix}│  ");
        self.condition
            .fmt_ast(f, source.as_ref(), condition_prefix.as_ref(), true)?;

        writeln!(f, "{child_prefix}└─ body:")?;
        let body_prefix = format!("{child_prefix}   ");
        self.body
            .fmt_ast(f, source.as_ref(), body_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for ForExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}For")?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        if let Some(index_var) = self.index_var.as_ref() {
            writeln!(f, "{child_prefix}├─ index variable:")?;
            let index_prefix = format!("{child_prefix}│  ");
            index_var.fmt_ast(f, source.as_ref(), index_prefix.as_ref(), true)?;
        }

        writeln!(f, "{child_prefix}├─ item variable:")?;
        let item_prefix = format!("{child_prefix}│  ");
        self.item_var
            .fmt_ast(f, source.as_ref(), item_prefix.as_ref(), true)?;

        writeln!(f, "{child_prefix}├─ iterable:")?;
        let iterable_prefix = format!("{child_prefix}│  ");
        self.iterable
            .fmt_ast(f, source.as_ref(), iterable_prefix.as_ref(), true)?;

        writeln!(f, "{child_prefix}└─ body:")?;
        let body_prefix = format!("{child_prefix}   ");
        self.body
            .fmt_ast(f, source.as_ref(), body_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for StructFieldInitExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        let field_name = &source.as_ref()[self.field_name.position.into_range()];
        writeln!(f, "{prefix}{branch}Field < {field_name} > with value:")?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");
        self.field_value
            .fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;

        Ok(())
    }
}

impl AstFmt for StructInitExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}StructInit with fields:")?;

        let child_branch = if is_last { "   " } else { "│  " };
        let child_prefix = format!("{prefix}{child_branch}");

        if self.fields.is_empty() {
            writeln!(f, "{child_prefix}└─ <empty>")?;
            return Ok(());
        }

        for (idx, field) in self.fields.iter().enumerate() {
            let is_last = idx == self.fields.len() - 1;
            field.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), is_last)?;
        }

        Ok(())
    }
}

impl AstFmt for ReturnExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        write!(f, "{prefix}{branch}Return")?;

        if let Some(value) = self.value.as_ref() {
            writeln!(f, " with value:")?;
            let child_branch = if is_last { "   " } else { "│  " };
            let child_prefix = format!("{prefix}{child_branch}");
            value.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;
        } else {
            writeln!(f)?;
        }

        Ok(())
    }
}

impl AstFmt for BreakExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        write!(f, "{prefix}{branch}Break")?;

        if let Some(value) = self.value.as_ref() {
            writeln!(f, " with value:")?;
            let child_branch = if is_last { "   " } else { "│  " };
            let child_prefix = format!("{prefix}{child_branch}");
            value.fmt_ast(f, source.as_ref(), child_prefix.as_ref(), true)?;
        } else {
            writeln!(f)?;
        }

        Ok(())
    }
}

impl AstFmt for ContinueExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}Continue")
    }
}

impl AstFmt for CommentExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}Comment")
    }
}

impl AstFmt for DocCommentExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        writeln!(f, "{prefix}{branch}Doc Comment")
    }
}

impl AstFmt for Expr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        match self {
            Expr::Block(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Binding(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Ident(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Number(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Bool(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::String(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Array(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Tuple(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Binary(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Unary(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::FunctionCall(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::ArrayAccess(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::MemberAccess(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::If(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::SemiColon(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::While(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::For(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::StructInit(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Return(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Break(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Continue(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::Comment(expr) => expr.fmt_ast(f, source, prefix, is_last),
            Expr::DocComment(expr) => expr.fmt_ast(f, source, prefix, is_last),
        }
    }
}

impl AstFmt for IdentifierExpr {
    fn fmt_ast<S>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        source: S,
        prefix: S,
        is_last: bool,
    ) -> std::fmt::Result
    where
        S: AsRef<str> + std::fmt::Display,
    {
        let branch = if is_last { "└─ " } else { "├─ " };
        let name = &source.as_ref()[self.position.into_range()];
        writeln!(f, "{prefix}{branch}Identifier < {name} >")
    }
}