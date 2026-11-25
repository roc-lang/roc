platform "glue-declared-name-collisions"
	requires {
		unused : {} -> {}
	}
	exposes [A, AB, Host]
	packages {}
	provides {
		"HostPingArgs": take_ping,
		"Slot": make_slot,
		"take_event": take_event,
		"take_helpers": take_helpers,
		"take_host_named": take_host_named,
		"take_joined": take_joined,
		"take_members": take_members,
		"take_runtime_named": take_runtime_named,
		"take_signal": take_signal,
		"take_signal_named": take_signal_named,
		"take_slot_it": take_slot_it,
		"take_slot_it_too": takeSlotIt,
	}
	hosted {
		"roc_host_fetch": Host.fetch!,
		"roc_host_load": Host.load!,
		"roc_host_ping": Host.ping!,
		"roc_host_send": Host.send!,
		"roc_host_send_it": Host.send_it!,
		"roc_host_send_it_too": Host.sendIt!,
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

## ZigGlue names this union's refcount helper like one its runtime support
## declares.
ErasedCallable := [Stored(Str), Absent]

take_event : Event -> U64
take_event = |_| 0

take_helpers : Helpers -> U64
take_helpers = |_| 0

take_signal : Signal -> U64
take_signal = |_| 0

take_signal_named : SignalRelease, SignalTag, SignalPayload, SignalDownPayload, SignalPayloadAlignment, SignalTagPayloadStorage -> U64
take_signal_named = |_, _, _, _, _, _| 0

take_runtime_named : RocStr, RocList, RocOps, HostedFunctions, Ordering, Layout, Option, VTable, ErasedCallable -> U64
take_runtime_named = |_, _, _, _, _, _, _, _, _| 0

take_host_named : Host.SendArgs, Host.FetchRetRecord, Host.LoadResult, Host.LoadResultTag -> U64
take_host_named = |_, _, _, _| 0

take_joined : A.BC, AB.C -> U64
take_joined = |_, _| 0

## A type named like a linker symbol the platform declares.
Slot := { place : U64 }

## Two provided definitions whose names join to one host name, so the
## aliases of their arguments would be named alike.
take_slot_it : Slot -> U64
take_slot_it = |_| 0

takeSlotIt : Key -> U64
takeSlotIt = |_| 0

make_slot : Slot -> Slot
make_slot = |slot| slot

take_ping : U64 -> U64
take_ping = |count| count

## Tags whose member names join in every generator (`HTTPError` and
## `Httperror`), in C only (`Auto` is escaped the way `AutoField` is spelled,
## and `Foo_bar` loses its underscore).
Members := [
	HTTPError(Str),
	Httperror(U16),
	HTTPPair(Str, U8),
	Httppair(U8, Str),
	Auto(U8),
	AutoField(U16),
	FooBar(U8),
	Foo_bar(U16),
	Plain,
]

## The same joins in a union without payloads, which each generator declares
## as an enum.
PureMembers := [HTTPError, Httperror, Plain]

## Fields whose member names join.
MemberFields := {
	fooBar : U8,
	foo_bar : U16,
	auto : U8,
	auto_field : Str,
	boxed : Box(PureMembers),
	listed : List(PureMembers),
}

take_members : Members, PureMembers, MemberFields -> U64
take_members = |_, _, _| 0
