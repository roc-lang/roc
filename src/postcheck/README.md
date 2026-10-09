# Post-check compilation

These stages consume immutable checked programs and explicit consumer demand.
They separate type/evidence instantiation, callable representation, and runtime
ownership so backends never reconstruct checking decisions or ARC behavior.

Evaluation demand must preserve the complete semantic-obligation graph without
making runtime procedure construction a prerequisite for checking. Its planner
requires exact producer-owned dependency summaries; constraint-only interface
summaries do not satisfy that contract.
