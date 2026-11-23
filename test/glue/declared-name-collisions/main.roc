platform "glue-declared-name-collisions"
	requires {
		unused : {} -> {}
	}
	exposes [A, AB, Host]
	packages {}
	provides {
		"take_event": take_event,
		"take_helpers": take_helpers,
		"take_host_named": take_host_named,
		"take_joined": take_joined,
		"take_runtime_named": take_runtime_named,
		"take_signal": take_signal,
		"take_signal_named": take_signal_named,
	}
	hosted {
		"roc_host_fetch": Host.fetch!,
		"roc_host_load": Host.load!,
		"roc_host_send": Host.send!,
	}
	targets: {}

import Host
import A
import AB

Key := { code : U32 }

## Each tag's payload is aliased as `Event<Tag>`, and `EventRelease` is also
## what a generator names the union's release helper.
Event := [Press(Key), Release(Key)]

## A tag for every other helper a generator names after a union.
Helpers := [
	Tag(Key),
	Payload(Key),
	PayloadAlignment(Key),
	TagPayloadStorage(Key),
	Both(U64, Str),
	BothPayload(Key),
]

Signal := [Up(Str), Down(U64, Str)]

## Types named like each helper a generator names after `Signal`.
SignalRelease := { held : Str }

SignalTag := { index : U64 }

SignalPayload := { bytes : List(U8) }

SignalDownPayload := { depth : U16 }

SignalPayloadAlignment := { boundary : U8 }

SignalTagPayloadStorage := { stored : I64 }

## Types named like the runtime support each generator emits, imports, or
## names its parameters and locals after.
RocStr := { text : Str }

RocList := [Empty, Full(Str)]

RocOps := { ops : U64 }

HostedFunctions := { count : U32 }

Ordering := [Less, Equal, Greater(Str)]

Layout := { size : U64 }

Option := [Missing, Present(Str)]

VTable := { slots : U16 }

take_event : Event -> U64
take_event = |_| 0

take_helpers : Helpers -> U64
take_helpers = |_| 0

take_signal : Signal -> U64
take_signal = |_| 0

take_signal_named : SignalRelease, SignalTag, SignalPayload, SignalDownPayload, SignalPayloadAlignment, SignalTagPayloadStorage -> U64
take_signal_named = |_, _, _, _, _, _| 0

take_runtime_named : RocStr, RocList, RocOps, HostedFunctions, Ordering, Layout, Option, VTable -> U64
take_runtime_named = |_, _, _, _, _, _, _, _| 0

take_host_named : Host.SendArgs, Host.FetchRetRecord, Host.LoadResult, Host.LoadResultTag -> U64
take_host_named = |_, _, _, _| 0

take_joined : A.BC, AB.C -> U64
take_joined = |_, _| 0
