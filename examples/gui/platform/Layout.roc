interface Layout
    exposes [ Layout ]
    imports [ Elem.{ Elem }, Bounds.{ Bounds } ]

## A quad tree of hitboxes with event handlers on them
BoundingBoxes : {}

## Given the available bounds and an elem, compute its layout.
## Do this in two passes: first, pass down "here is the available space you can take up"
## and then pass back up "here is how much space I decided to take up"
fromElem : Elem state, Bounds -> { bounds: Bounds, bbs: BoundingBoxes }
fromElem = \elem, available ->
    when elem is
        Button config child ->
            # Tell the child it can have max(0, bounds - padding) space, then report back
            # that we took up (childBounds + padding) space - should be no chance of exceeding that
            # bc to exceed that, would have had to have child exceed that space, so...
            # can we issue drawing commands immediately after we hear back on the final bounds? *should* we?
            # Also, once we're about to report back our bounds, might as well just jump into the quad tree, yeah?
            # Ooh! Don't need to jump in the quad tree at all unless we have an onClick or onMouseOver etc., right?
        Text str ->
            # TODO in order to do this one, we need HarfBuzz! Need to measure width of each glyph.
            # Oh - I guess detecting when you're in between letters is actually...straightforward?
            # All we really need is to do the calculations and then make small bounding boxen based on that.
            # Maybe we could do that lazily though? Like don't do it until the first mouseover?
            # Because doing that for every single text area on every single render sounds...expensive.
            #
            # ok so what this one needs to do is: given the bounds, just keep iterating and placing glyphs until
            # we run out of space...(at what point do we wrap? I guess when we run out of width!)
            # should we actually store the bounds of each letter so that mouseover works? I'm very unsure!
            # maybe ask Hector about how this works? About layout stuff in general?
        Col (List (Elem state)) ->
            # TODO tell the first one it can take up as much space as it likes, then ask it how much
            # space it would actually like to take up. Once that one has reported back, subtract that
            # from the remaining amount of space (well, set new left = old right) and continue with
            # the next one until we end up with 0 space left.
        Row (List (Elem state)),
            # Same as col but width instead of height
        Lazy (state -> Elem state),
            # Force the thunk
        Cached (Result { state, elem : Elem state } [ NotCached ] -> { state, elem : Elem state }),
        # TODO FIXME: using this definition of Lazy causes a stack overflow in the compiler!
        #Lazy (Result (Cached state) [ NotCached ] -> Cached state),
        None,
    ]
