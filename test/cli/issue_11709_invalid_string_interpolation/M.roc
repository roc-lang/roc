# Its string interpolates the reserved word `module`, which does not parse as
# an expression.
M := [].{
	s = "${module}"
}
