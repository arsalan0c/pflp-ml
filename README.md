# pflp-ml

An embedded DSL for probabilistic programming in OCaml, based on 
1. [PFLP](https://arxiv.org/pdf/1905.07212.pdf) by Dylus, Christiansen and Teegen 
2. [kanren](https://github.com/arsalanc-v2/kanren)

## Installation
You will need `opam`, `OCaml` and `dune` .

The code can be obtained with:
```
git clone --recursive https://github.com/arsalanc-v2/pflp-ml.git
```

## Tests
The tests (examples) in `./examples` can be run with:
```
cd pflp-ml
dune runtest
```
## Interface
```OCaml
exception PflpFailure of Base.string
val failwith : Base.string -> 'a

type probability = Base.float
val prob : Base.float -> Mk.Micro.term

val dist :
  Mk.Micro.term ->
  Base.float ->
  Mk.Micro.substitution * Mk.Micro.var -> Mk.Micro.state Mk.Micro.stream

val dist_to_probability : Mk.Micro.term -> Base.float

val dist_to_event : Mk.Micro.term -> Mk.Micro.term

val certainly :
  Mk.Micro.term ->
  Mk.Micro.substitution * Mk.Micro.var -> Mk.Micro.state Mk.Micro.stream

val ( $ ) :
  ('a -> 'b Mk.Micro.stream) ->
  ('a -> 'b Mk.Micro.stream) -> 'a -> 'b Mk.Micro.stream

val member :
  (Mk.Micro.state -> 'a Mk.Micro.stream) Base.List.t ->
  Mk.Micro.state -> 'a Mk.Micro.stream

val enum :
  Mk.Micro.term Base.List.t ->
  Base.float Base.List.t -> Mk.Micro.state -> Mk.Micro.state Mk.Micro.stream

val uniform :
  Mk.Micro.term Base.List.t ->
  Mk.Micro.state -> Mk.Micro.state Mk.Micro.stream

val ( >>>= ) :
  ('a -> (('b, Mk.Micro.term, 'c) Base.Map.t * 'd) Mk.Micro.stream) ->
  (Mk.Micro.term -> Mk.Micro.state -> Mk.Micro.state Mk.Micro.stream) ->
  'a -> Mk.Micro.state Mk.Micro.stream

val join_with :
  (Mk.Micro.term -> Mk.Micro.term -> Mk.Micro.term) ->
  ('a -> (('b, Mk.Micro.term, 'c) Base.Map.t * 'd) Mk.Micro.stream) ->
  (Mk.Micro.state ->
   (('e, Mk.Micro.term, 'f) Base.Map.t * 'g) Mk.Micro.stream) ->
  'a -> Mk.Micro.state Mk.Micro.stream

val filter_dist :
  (Mk.Micro.term -> bool) ->
  ('a -> (('b, Mk.Micro.term, 'c) Base.Map.t * 'd) Mk.Micro.stream) ->
  'a -> Mk.Micro.state Mk.Micro.stream

val ( $$ ) :
  (Mk.Micro.term -> bool) ->
  ('a -> (('b, Mk.Micro.term, 'c) Base.Map.t * 'd) Mk.Micro.stream) ->
  'a -> Base__Float.t
```

## References
- [pflp reference implementation](https://github.com/finnteegen/pflp/)


