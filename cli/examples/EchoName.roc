succeed = \val -> Success val


fail = \val -> Failure val


echo = \str -> Echo fail succeed str


readInput = Read fail succeed


map = \convert, task ->
    after task \output ->
    succeed (convert output)


mapErr = \convert, task ->
    fallback task \err ->
    fail (convert err)


after = \task, cont ->
    when task is
        Success val -> cont val
        Failure val -> Failure val

        Echo onFailure prevCont str ->
            Echo
                (\ioErr -> after (onFailure ioErr) cont)
                (\{} -> after (prevCont {}) cont)
                str

        Read onFailure prevCont ->
            Read
                (\ioErr -> after (onFailure ioErr) cont)
                (\str -> after (prevCont str) cont)


fallback = \task onFailure ->
    when task is
        Success val -> Success val
        Failure val -> onFailure val

        Echo prevOnFailure cont str ->
            Echo
                (\ioErr -> fallback (prevOnFailure ioErr) onFailure)
                (\{} -> fallback (cont {}) onFailure)
                str

        Read prevOnFailure cont ->
            Read
                (\ioErr -> fallback (prevOnFailure ioErr) onFailure)
                (\str -> fallback (cont str) onFailure)


###############################################################
# In the future everything above here will be in a platform. #
###############################################################

program =
    after (echo "What is your first name?") \{} ->
    after readInput \firstName ->
    after (echo "Hi \(firstName)! What is your last name?") \{} ->
    after readInput \lastName ->
    echo "Your full name is: \(firstName) \(lastName)"

program
