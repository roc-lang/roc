## Pipeline module — the top-level orchestrator.
## Imports Field, Transform, and Util, creating the deepest point in the import tree.
##
## Full dependency graph:
##   Pipeline → Field → Validator → Util
##   Pipeline → Field → Util
##   Pipeline → Transform → Util
##   Pipeline → Util

import Field
import Transform
import Util

Pipeline := {
	fields : List(Field.Field),
	transforms : List(Transform.Transform),
}.{

	## Add a field to the pipeline.
	add_field : Pipeline, Field.Field -> Pipeline
	add_field = |pipeline, f|
		{ ..pipeline, fields: pipeline.fields.append(f) }

	## Add a transformation step to the pipeline.
	add_transform : Pipeline, Transform.Transform -> Pipeline
	add_transform = |pipeline, t|
		{ ..pipeline, transforms: pipeline.transforms.append(t) }

	## Run the pipeline: validate all fields, then transform the valid values.
	run : Pipeline -> List({ name : Str, result : Try(Str, Str) })
	run = |pipeline| {
		validated = Field.validate_all(pipeline.fields)
		transform_results(validated, pipeline.transforms)
	}

	## Run the pipeline and produce a human-readable report.
	report : Pipeline -> Str
	report = |pipeline| {
		results = run(pipeline)
		format_results(results)
	}
}

transform_results : List({ name : Str, result : Try(Str, Str) }), List(Transform.Transform) -> List({ name : Str, result : Try(Str, Str) })
transform_results = |entries, transforms|
	entries.map(
		|entry|
			match entry.result {
				Ok(val) => { name: entry.name, result: Ok(Transform.apply_all(transforms, val)) }
				Err(e) => { name: entry.name, result: Err(e) }
			},
	)

format_results : List({ name : Str, result : Try(Str, Str) }) -> Str
format_results = |results| {
	lines = results.map(
		|entry|
			match entry.result {
				Ok(val) => "${entry.name}: ${val}"
				Err(e) => "${entry.name}: ERROR - ${e}"
			},
	)

	Util.join_with(lines, "\n")
}
