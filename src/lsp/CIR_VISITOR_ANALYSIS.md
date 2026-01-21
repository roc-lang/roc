# CIR Visitor Analysis Report

This document analyzes the duplicated CIR traversal patterns in `syntax.zig` to understand the requirements for a generic `CirVisitor`.

## Summary of Analyzed Functions

### 1. `findLookupAtOffset` (line 1019)

**Purpose:** Find the most specific lookup (local/external variable or dot access) at a given byte offset.

**Returns:** `?CIR.Expr.Idx`

**Callback/Action:** 
- For `e_lookup_local` and `e_lookup_external`: Records the expr_idx if its region contains the offset and is smaller than the best match so far
- For `e_dot_access`: Checks if cursor is on the field_name_region and records if smaller

**Expression types handled for recursion:**
- `e_lambda` → recurse into `body`
- `e_closure` → recurse into `lambda_idx`
- `e_block` → recurse into statements (via `getStatementParts`) + `final_expr`
- `e_if` → recurse into branches (`cond`, `body`) + `final_else`
- `e_match` → recurse into `cond` + branches (`value`, `guard`)
- `e_call` → recurse into `func` + `args`
- `e_binop` → recurse into `lhs`, `rhs`
- `e_dot_access` → recurse into `receiver` + `args`
- `e_str` → recurse into segments
- `e_list` → recurse into `elems`
- `e_tuple` → recurse into `elems`
- `e_record` → recurse into field values

---

### 2. `findTypeAnnoInExpr` (line 1356)

**Purpose:** Find type annotations nested within expressions at a given offset.

**Returns:** `?DefinitionResult` (uri + range)

**Callback/Action:** 
- For block statements with annotations, calls `findTypeAnnoAtOffset` on the type annotation

**Expression types handled for recursion:**
- `e_block` → statements (extracts type annos from `s_decl`, `s_decl_gen`, `s_var`, `s_type_anno`, `s_alias_decl`, `s_nominal_decl`) + `final_expr`
- `e_lambda` → recurse into `body`
- `e_closure` → recurse into `lambda_idx`
- `e_if` → recurse into branches (`cond`, `body`) + `final_else`
- `e_match` → recurse into `cond` + branches (`value`, `guard`)
- `e_call` → recurse into `func` + `args`

---

### 3. `findNestedTypeAtOffset` (line 1751)

**Purpose:** Find the narrowest expression or pattern containing the target offset to determine its type variable.

**Returns:** `?TypeAtOffsetResult` (type_var + region)

**Callback/Action:**
- Calls `checkExprAndRecurse` which checks if region contains offset, updates best match, and recurses

**Expression types handled for recursion:**
- `e_lambda` → recurse into `body`
- `e_closure` → recurse into `lambda_idx`
- `e_block` → statements + patterns + annotations + `final_expr`
- `e_if` → recurse into branches (`cond`, `body`) + `final_else`
- `e_match` → recurse into `cond` + branches (patterns, `value`, `guard`)
- `e_call` → recurse into `func` + `args`
- `e_list` → recurse into `elems`
- `e_record` → recurse into field values + `ext`
- `e_tuple` → recurse into `elems`
- `e_binop` → recurse into `lhs`, `rhs`
- `e_unary_minus` → recurse into `expr`
- `e_unary_not` → recurse into `expr`
- `e_str` → recurse into segments
- `e_dot_access` → recurse into `receiver` + `args`

---

### 4. `collectLookupsInExpr` (line 2436)

**Purpose:** Collect all references to a specific pattern (variable) for find-references.

**Returns:** `void` (appends to `regions: *std.ArrayList(LspRange)`)

**Callback/Action:**
- For `e_lookup_local`: if `lookup.pattern_idx == target_pattern`, append region to list

**Expression types handled for recursion:**
- `e_closure` → recurse into `lambda_idx`
- `e_lambda` → recurse into `body`
- `e_call` → recurse into `func` + `args`
- `e_block` → statements + `final_expr`
- `e_if` → recurse into branches (`cond`, `body`) + `final_else`
- `e_match` → recurse into `cond` + branches (`value`, `guard`)
- `e_list` → recurse into `elems`
- `e_tuple` → recurse into `elems`
- `e_record` → recurse into field values

---

### 5. `findDotReceiverTypeVarInExpr` (line 3849)

**Purpose:** Find the type variable of a dot access receiver at a given offset (for method completion).

**Returns:** `?types.Var`

**Callback/Action:**
- For `e_dot_access`: if cursor is on `field_name_region`, return the type var of the receiver

**Expression types handled for recursion (most comprehensive):**
- `e_lambda` → recurse into `body`
- `e_closure` → recurse into `lambda_idx`
- `e_block` → statements + `final_expr`
- `e_if` → recurse into branches (`cond`, `body`) + `final_else`
- `e_match` → recurse into `cond` + branches (`value`, `guard`)
- `e_call` → recurse into `func` + `args`
- `e_binop` → recurse into `lhs`, `rhs`
- `e_unary_minus` → recurse into `expr`
- `e_unary_not` → recurse into `expr`
- `e_list` → recurse into `elems`
- `e_tuple` → recurse into `elems`
- `e_record` → recurse into field values + `ext`
- `e_dot_access` → recurse into `receiver` + `args`
- `e_str` → recurse into segments
- `e_tag` → recurse into `args`
- `e_nominal` → recurse into `backing_expr`
- `e_nominal_external` → recurse into `backing_expr`
- `e_dbg` → recurse into `expr`
- `e_expect` → recurse into `body`
- `e_return` → recurse into `expr`
- `e_for` → recurse into `expr` + `body`
- `e_type_var_dispatch` → recurse into `args`

---

## Complete CIR Type Lists

### Expression Variants (`CIR.Expr`)

Based on Expression.zig, here are ALL expression variants:

**Literals:**
- `e_num` - integer literal
- `e_frac_f32` - f32 literal
- `e_frac_f64` - f64 literal
- `e_dec` - decimal literal
- `e_dec_small` - small decimal literal
- `e_typed_int` - typed integer literal (e.g., `123.U64`)
- `e_typed_frac` - typed fractional literal
- `e_str_segment` - string segment
- `e_str` - string with interpolation → contains `Expr.Span`
- `e_empty_list` - empty list `[]`
- `e_list` - list literal → contains `Expr.Span` of elems
- `e_empty_record` - empty record `{}`
- `e_record` - record literal → contains `RecordField.Span` + optional `Expr.Idx` ext
- `e_tuple` - tuple literal → contains `Expr.Span` of elems

**Variables:**
- `e_lookup_local` - local variable lookup → contains `Pattern.Idx`
- `e_lookup_external` - external module lookup → contains module info
- `e_lookup_required` - required identifier from platform

**Tags:**
- `e_tag` - tag with arguments → contains `Expr.Span` of args
- `e_zero_argument_tag` - tag with no arguments
- `e_nominal` - nominal type application → contains `Expr.Idx` backing_expr
- `e_nominal_external` - external nominal type → contains `Expr.Idx` backing_expr

**Functions/Lambdas:**
- `e_lambda` - lambda expression → contains `Pattern.Span` args + `Expr.Idx` body
- `e_closure` - closure (lambda with captures) → contains `Expr.Idx` lambda_idx + `Capture.Span`
- `e_call` - function call → contains `Expr.Idx` func + `Expr.Span` args
- `e_hosted_lambda` - hosted function from platform
- `e_low_level_lambda` - low-level builtin operation

**Control Flow:**
- `e_if` - if expression → contains `IfBranch.Span` + `Expr.Idx` final_else
- `e_match` - match expression → contains `Match` (cond + branches)
- `e_block` - block expression → contains `Statement.Span` + `Expr.Idx` final_expr
- `e_for` - for loop → contains `Pattern.Idx` + 2x `Expr.Idx`

**Operations:**
- `e_binop` - binary operation → contains 2x `Expr.Idx`
- `e_unary_minus` - unary minus → contains `Expr.Idx`
- `e_unary_not` - unary not → contains `Expr.Idx`
- `e_dot_access` - field/method access → contains `Expr.Idx` receiver + optional `Expr.Span` args

**Special:**
- `e_runtime_error` - error placeholder
- `e_crash` - crash expression
- `e_dbg` - debug expression → contains `Expr.Idx`
- `e_expect` - expect assertion → contains `Expr.Idx` body
- `e_ellipsis` - `...` placeholder
- `e_anno_only` - standalone type annotation
- `e_return` - early return → contains `Expr.Idx`
- `e_type_var_dispatch` - type variable method dispatch → contains `Expr.Span` args

---

### Statement Variants (`CIR.Statement`)

**Declarations:**
- `s_decl` - simple declaration → pattern + expr + optional anno
- `s_decl_gen` - generalized declaration → pattern + expr + optional anno
- `s_var` - rebindable declaration → pattern + expr + optional anno
- `s_reassign` - reassignment → pattern + expr

**Type Declarations:**
- `s_alias_decl` - alias type declaration
- `s_nominal_decl` - nominal type declaration
- `s_type_anno` - standalone type annotation
- `s_type_var_alias` - type variable alias

**Control Flow:**
- `s_for` - for loop statement → pattern + 2x expr
- `s_while` - while loop → 2x expr
- `s_break` - break
- `s_return` - return → expr

**Other:**
- `s_expr` - expression statement → expr
- `s_expect` - expect assertion → expr
- `s_crash` - crash statement
- `s_dbg` - debug statement → expr
- `s_import` - import statement
- `s_runtime_error` - error placeholder

---

### Pattern Variants (`CIR.Pattern`)

- `assign` - identifier binding
- `as` - alias pattern → contains nested `Pattern.Idx`
- `applied_tag` - tag with arguments → contains `Pattern.Span` args
- `nominal` - nominal pattern → contains `Pattern.Idx` backing_pattern
- `nominal_external` - external nominal → contains `Pattern.Idx` backing_pattern
- `record_destructure` - record destructure → contains `RecordDestruct.Span`
- `list` - list pattern → contains `Pattern.Span` + optional rest
- `tuple` - tuple pattern → contains `Pattern.Span`
- `num_literal` - number literal pattern
- `small_dec_literal` - small decimal pattern
- `dec_literal` - decimal pattern
- `frac_f32_literal` - f32 literal pattern
- `frac_f64_literal` - f64 literal pattern
- `str_literal` - string literal pattern
- `underscore` - wildcard pattern
- `runtime_error` - error placeholder

---

### TypeAnno Variants (`CIR.TypeAnno`)

- `apply` - type application (e.g., `List(Str)`)
- `rigid_var` - type variable
- `rigid_var_lookup` - reference to rigid var
- `underscore` - inferred type `_`
- `lookup` - basic type name
- `tag_union` - tag union type
- `tag` - single tag in union
- `tuple` - tuple type
- `record` - record type
- `fn` - function type
- `parens` - parenthesized type
- `malformed` - error placeholder

---

## Common Traversal Pattern

All functions follow this pattern:

```zig
fn traverseExpr(expr_idx: CIR.Expr.Idx, ...) ?Result {
    const expr = module_env.store.getExpr(expr_idx);
    var result: ?Result = null;
    
    // 1. Check if this expression matches the search criteria
    // 2. Perform action/callback on matching expressions
    
    // 3. Recurse into child expressions
    switch (expr) {
        .e_lambda => |lambda| {
            if (self.traverseExpr(lambda.body, ...)) |r| result = r;
        },
        .e_closure => |closure| {
            if (self.traverseExpr(closure.lambda_idx, ...)) |r| result = r;
        },
        .e_block => |block| {
            for (stmts) |stmt_idx| {
                // Handle statement parts (pattern, expr, expr2)
            }
            if (self.traverseExpr(block.final_expr, ...)) |r| result = r;
        },
        // ... etc
    }
    
    return result;
}
```

---

## Suggested CirVisitor Interface

```zig
pub const CirVisitor = struct {
    const Self = @This();
    
    /// Context passed through the visitor
    ctx: *anyopaque,
    
    /// Hooks for each visitable node type
    visit_expr_pre: ?*const fn (*anyopaque, CIR.Expr.Idx, CIR.Expr) VisitAction = null,
    visit_expr_post: ?*const fn (*anyopaque, CIR.Expr.Idx, CIR.Expr) void = null,
    visit_stmt_pre: ?*const fn (*anyopaque, CIR.Statement.Idx, CIR.Statement) VisitAction = null,
    visit_stmt_post: ?*const fn (*anyopaque, CIR.Statement.Idx, CIR.Statement) void = null,
    visit_pattern_pre: ?*const fn (*anyopaque, CIR.Pattern.Idx, CIR.Pattern) VisitAction = null,
    visit_pattern_post: ?*const fn (*anyopaque, CIR.Pattern.Idx, CIR.Pattern) void = null,
    visit_type_anno_pre: ?*const fn (*anyopaque, CIR.TypeAnno.Idx, CIR.TypeAnno) VisitAction = null,
    visit_type_anno_post: ?*const fn (*anyopaque, CIR.TypeAnno.Idx, CIR.TypeAnno) void = null,
    
    pub const VisitAction = enum {
        /// Continue traversing children
        @"continue",
        /// Skip children of this node
        skip_children,
        /// Stop traversal entirely
        stop,
    };
    
    pub fn walkExpr(self: *Self, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) void {
        const expr = module_env.store.getExpr(expr_idx);
        
        // Pre-visit hook
        if (self.visit_expr_pre) |hook| {
            switch (hook(self.ctx, expr_idx, expr)) {
                .@"continue" => {},
                .skip_children => {
                    if (self.visit_expr_post) |post| post(self.ctx, expr_idx, expr);
                    return;
                },
                .stop => return,
            }
        }
        
        // Walk children based on expression type
        switch (expr) {
            .e_lambda => |lambda| {
                for (module_env.store.slicePatterns(lambda.args)) |arg| {
                    self.walkPattern(module_env, arg);
                }
                self.walkExpr(module_env, lambda.body);
            },
            .e_closure => |closure| {
                self.walkExpr(module_env, closure.lambda_idx);
            },
            .e_block => |block| {
                for (module_env.store.sliceStatements(block.stmts)) |stmt| {
                    self.walkStatement(module_env, stmt);
                }
                self.walkExpr(module_env, block.final_expr);
            },
            .e_if => |if_expr| {
                for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    self.walkExpr(module_env, branch.cond);
                    self.walkExpr(module_env, branch.body);
                }
                self.walkExpr(module_env, if_expr.final_else);
            },
            .e_match => |match_expr| {
                self.walkExpr(module_env, match_expr.cond);
                for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    // Walk branch patterns
                    for (module_env.store.sliceMatchBranchPatterns(branch.patterns)) |pat_idx| {
                        const bp = module_env.store.getMatchBranchPattern(pat_idx);
                        self.walkPattern(module_env, bp.pattern);
                    }
                    self.walkExpr(module_env, branch.value);
                    if (branch.guard) |guard| self.walkExpr(module_env, guard);
                }
            },
            .e_call => |call| {
                self.walkExpr(module_env, call.func);
                for (module_env.store.sliceExpr(call.args)) |arg| {
                    self.walkExpr(module_env, arg);
                }
            },
            .e_binop => |binop| {
                self.walkExpr(module_env, binop.lhs);
                self.walkExpr(module_env, binop.rhs);
            },
            .e_unary_minus, .e_unary_not => |u| {
                self.walkExpr(module_env, u.expr);
            },
            .e_dot_access => |dot| {
                self.walkExpr(module_env, dot.receiver);
                if (dot.args) |args| {
                    for (module_env.store.sliceExpr(args)) |arg| {
                        self.walkExpr(module_env, arg);
                    }
                }
            },
            .e_list => |list| {
                for (module_env.store.sliceExpr(list.elems)) |elem| {
                    self.walkExpr(module_env, elem);
                }
            },
            .e_tuple => |tuple| {
                for (module_env.store.sliceExpr(tuple.elems)) |elem| {
                    self.walkExpr(module_env, elem);
                }
            },
            .e_record => |rec| {
                for (module_env.store.sliceRecordFields(rec.fields)) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    self.walkExpr(module_env, field.value);
                }
                if (rec.ext) |ext| self.walkExpr(module_env, ext);
            },
            .e_str => |str| {
                for (module_env.store.sliceExpr(str.span)) |seg| {
                    self.walkExpr(module_env, seg);
                }
            },
            .e_tag => |tag| {
                for (module_env.store.sliceExpr(tag.args)) |arg| {
                    self.walkExpr(module_env, arg);
                }
            },
            .e_nominal, .e_nominal_external => |nom| {
                self.walkExpr(module_env, nom.backing_expr);
            },
            .e_dbg => |dbg| self.walkExpr(module_env, dbg.expr),
            .e_expect => |exp| self.walkExpr(module_env, exp.body),
            .e_return => |ret| self.walkExpr(module_env, ret.expr),
            .e_for => |for_| {
                self.walkPattern(module_env, for_.patt);
                self.walkExpr(module_env, for_.expr);
                self.walkExpr(module_env, for_.body);
            },
            .e_type_var_dispatch => |tvd| {
                for (module_env.store.sliceExpr(tvd.args)) |arg| {
                    self.walkExpr(module_env, arg);
                }
            },
            .e_hosted_lambda, .e_low_level_lambda => |hl| {
                for (module_env.store.slicePatterns(hl.args)) |arg| {
                    self.walkPattern(module_env, arg);
                }
                self.walkExpr(module_env, hl.body);
            },
            // Leaf nodes - no children to traverse
            .e_num, .e_frac_f32, .e_frac_f64, .e_dec, .e_dec_small,
            .e_typed_int, .e_typed_frac, .e_str_segment,
            .e_empty_list, .e_empty_record,
            .e_lookup_local, .e_lookup_external, .e_lookup_required,
            .e_zero_argument_tag, .e_runtime_error, .e_crash,
            .e_ellipsis, .e_anno_only => {},
        }
        
        // Post-visit hook
        if (self.visit_expr_post) |post| post(self.ctx, expr_idx, expr);
    }
    
    pub fn walkStatement(self: *Self, module_env: *ModuleEnv, stmt_idx: CIR.Statement.Idx) void {
        const stmt = module_env.store.getStatement(stmt_idx);
        
        if (self.visit_stmt_pre) |hook| {
            switch (hook(self.ctx, stmt_idx, stmt)) {
                .@"continue" => {},
                .skip_children => {
                    if (self.visit_stmt_post) |post| post(self.ctx, stmt_idx, stmt);
                    return;
                },
                .stop => return,
            }
        }
        
        switch (stmt) {
            .s_decl => |d| {
                self.walkPattern(module_env, d.pattern);
                self.walkExpr(module_env, d.expr);
                if (d.anno) |a| self.walkAnnotation(module_env, a);
            },
            .s_decl_gen => |d| {
                self.walkPattern(module_env, d.pattern);
                self.walkExpr(module_env, d.expr);
                if (d.anno) |a| self.walkAnnotation(module_env, a);
            },
            .s_var => |v| {
                self.walkPattern(module_env, v.pattern_idx);
                self.walkExpr(module_env, v.expr);
                if (v.anno) |a| self.walkAnnotation(module_env, a);
            },
            .s_reassign => |r| {
                self.walkPattern(module_env, r.pattern_idx);
                self.walkExpr(module_env, r.expr);
            },
            .s_for => |f| {
                self.walkPattern(module_env, f.patt);
                self.walkExpr(module_env, f.expr);
                self.walkExpr(module_env, f.body);
            },
            .s_while => |w| {
                self.walkExpr(module_env, w.cond);
                self.walkExpr(module_env, w.body);
            },
            .s_expr => |e| self.walkExpr(module_env, e.expr),
            .s_expect => |e| self.walkExpr(module_env, e.body),
            .s_dbg => |d| self.walkExpr(module_env, d.expr),
            .s_return => |r| self.walkExpr(module_env, r.expr),
            .s_type_anno => |t| self.walkTypeAnno(module_env, t.anno),
            .s_alias_decl => |a| self.walkTypeAnno(module_env, a.anno),
            .s_nominal_decl => |n| self.walkTypeAnno(module_env, n.anno),
            .s_crash, .s_break, .s_import, .s_type_var_alias, .s_runtime_error => {},
        }
        
        if (self.visit_stmt_post) |post| post(self.ctx, stmt_idx, stmt);
    }
    
    pub fn walkPattern(self: *Self, module_env: *ModuleEnv, pattern_idx: CIR.Pattern.Idx) void {
        const pattern = module_env.store.getPattern(pattern_idx);
        
        if (self.visit_pattern_pre) |hook| {
            switch (hook(self.ctx, pattern_idx, pattern)) {
                .@"continue" => {},
                .skip_children => {
                    if (self.visit_pattern_post) |post| post(self.ctx, pattern_idx, pattern);
                    return;
                },
                .stop => return,
            }
        }
        
        switch (pattern) {
            .as => |a| self.walkPattern(module_env, a.pattern),
            .applied_tag => |t| {
                for (module_env.store.slicePatterns(t.args)) |arg| {
                    self.walkPattern(module_env, arg);
                }
            },
            .nominal, .nominal_external => |n| {
                self.walkPattern(module_env, n.backing_pattern);
            },
            .record_destructure => |r| {
                for (module_env.store.sliceRecordDestructs(r.destructs)) |d_idx| {
                    const destruct = module_env.store.getRecordDestruct(d_idx);
                    self.walkPattern(module_env, destruct.kind.toPatternIdx());
                }
            },
            .list => |l| {
                for (module_env.store.slicePatterns(l.patterns)) |p| {
                    self.walkPattern(module_env, p);
                }
                if (l.rest_info) |rest| {
                    if (rest.pattern) |rp| self.walkPattern(module_env, rp);
                }
            },
            .tuple => |t| {
                for (module_env.store.slicePatterns(t.patterns)) |p| {
                    self.walkPattern(module_env, p);
                }
            },
            // Leaf patterns
            .assign, .num_literal, .small_dec_literal, .dec_literal,
            .frac_f32_literal, .frac_f64_literal, .str_literal,
            .underscore, .runtime_error => {},
        }
        
        if (self.visit_pattern_post) |post| post(self.ctx, pattern_idx, pattern);
    }
    
    pub fn walkTypeAnno(self: *Self, module_env: *ModuleEnv, anno_idx: CIR.TypeAnno.Idx) void {
        const anno = module_env.store.getTypeAnno(anno_idx);
        
        if (self.visit_type_anno_pre) |hook| {
            switch (hook(self.ctx, anno_idx, anno)) {
                .@"continue" => {},
                .skip_children => {
                    if (self.visit_type_anno_post) |post| post(self.ctx, anno_idx, anno);
                    return;
                },
                .stop => return,
            }
        }
        
        switch (anno) {
            .apply => |a| {
                for (module_env.store.sliceTypeAnnos(a.args)) |arg| {
                    self.walkTypeAnno(module_env, arg);
                }
            },
            .rigid_var_lookup => |r| self.walkTypeAnno(module_env, r.ref),
            .tag_union => |tu| {
                for (module_env.store.sliceTypeAnnos(tu.tags)) |tag| {
                    self.walkTypeAnno(module_env, tag);
                }
                if (tu.ext) |ext| self.walkTypeAnno(module_env, ext);
            },
            .tag => |t| {
                for (module_env.store.sliceTypeAnnos(t.args)) |arg| {
                    self.walkTypeAnno(module_env, arg);
                }
            },
            .tuple => |t| {
                for (module_env.store.sliceTypeAnnos(t.elems)) |elem| {
                    self.walkTypeAnno(module_env, elem);
                }
            },
            .record => |r| {
                for (module_env.store.sliceAnnoRecordFields(r.fields)) |f_idx| {
                    const field = module_env.store.getAnnoRecordField(f_idx);
                    self.walkTypeAnno(module_env, field.ty);
                }
                if (r.ext) |ext| self.walkTypeAnno(module_env, ext);
            },
            .@"fn" => |f| {
                for (module_env.store.sliceTypeAnnos(f.args)) |arg| {
                    self.walkTypeAnno(module_env, arg);
                }
                self.walkTypeAnno(module_env, f.ret);
            },
            .parens => |p| self.walkTypeAnno(module_env, p.anno),
            // Leaf types
            .rigid_var, .underscore, .lookup, .malformed => {},
        }
        
        if (self.visit_type_anno_post) |post| post(self.ctx, anno_idx, anno);
    }
    
    fn walkAnnotation(self: *Self, module_env: *ModuleEnv, anno_idx: CIR.Annotation.Idx) void {
        const annotation = module_env.store.getAnnotation(anno_idx);
        self.walkTypeAnno(module_env, annotation.anno);
        if (annotation.where) |where_span| {
            for (module_env.store.sliceWhereClauses(where_span)) |clause_idx| {
                const clause = module_env.store.getWhereClause(clause_idx);
                switch (clause) {
                    .w_method => |m| {
                        self.walkTypeAnno(module_env, m.var_);
                        for (module_env.store.sliceTypeAnnos(m.args)) |arg| {
                            self.walkTypeAnno(module_env, arg);
                        }
                        self.walkTypeAnno(module_env, m.ret);
                    },
                    .w_alias => |a| self.walkTypeAnno(module_env, a.var_),
                    .w_malformed => {},
                }
            }
        }
    }
};
```

---

## Key Differences Between Functions

| Function | Checks Region? | Returns Early? | Accumulates? | Handles Patterns? |
|----------|---------------|----------------|--------------|-------------------|
| findLookupAtOffset | Yes | No (best match) | No | No |
| findTypeAnnoInExpr | Yes | Yes (first match) | No | No |
| findNestedTypeAtOffset | Yes | No (best match) | No | Yes |
| collectLookupsInExpr | No | No | Yes (list) | No |
| findDotReceiverTypeVarInExpr | Yes | No (best match) | No | No |

---

## Hooks Needed for Each Function

1. **findLookupAtOffset**: `visit_expr_pre` with region check, looks for `e_lookup_local`, `e_lookup_external`, `e_dot_access`

2. **findTypeAnnoInExpr**: `visit_stmt_pre` to extract annotations, `visit_type_anno_pre` for type lookup

3. **findNestedTypeAtOffset**: `visit_expr_pre` with region check and best-match logic, `visit_pattern_pre` for patterns

4. **collectLookupsInExpr**: `visit_expr_pre` filtering for `e_lookup_local` with pattern match

5. **findDotReceiverTypeVarInExpr**: `visit_expr_pre` with region check, specifically for `e_dot_access` field name region
