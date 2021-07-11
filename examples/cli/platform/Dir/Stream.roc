

# NOTE: need to figure out what to do about O_EXCL with tempfiles
#
# Uses for this, from the docs: https://www.man7.org/linux/man-pages/man2/open.2.html
#
# * Improved tmpfile(3) functionality: race-free creation
#   of temporary files that (1) are automatically deleted
#   when closed; (2) can never be reached via any pathname;
#   (3) are not subject to symlink attacks; and (4) do not
#   require the caller to devise unique names.

# *  Creating a file that is initially invisible, which is
#   then populated with data and adjusted to have
#   appropriate filesystem attributes (fchown(2),
#   fchmod(2), fsetxattr(2), etc.)  before being atomically
#   linked into the filesystem in a fully formed state
#   (using linkat(2) as described above).

# NOTE: There is no openTempRead or openTempReadClose. Why would anyone want to
# open a (necessarily empty) tempfile with only read permissions?

openTempWrite : Str (Stream [ Write ] -> Task ok []err) -> Task ok [ OpenTempFailed OpenErr Str ]err

openTempWriteClose : Str -> Task (Stream [ Write, Close ]) [ OpenTempFailed OpenErr Str ]*

openTempAppend : Str, (Stream [ Append ] -> Task ok []err) -> Task ok [ OpenFailed OpenErr Str ]err

openTempAppendClose : Str -> Task (Stream [ Append, Close ]) [ OpenFailed OpenErr Str ]*

openTempReadWrite : Str, (Stream [ Read, Write ] -> Task ok []err) -> Task ok [ OpenFailed OpenErr Str ]err

openTempReadWriteClose : Str -> Task (Stream [ Read, Write, Close ]) [ OpenFailed OpenErr Str ]*

openTempReadAppend : Str, (Stream [ Read, Append ] -> Task ok []err) -> Task ok [ OpenFailed OpenErr Str ]err

openTempReadAppendClose : Str -> Task (Stream [ Read, Append, Close ]) [ OpenFailed OpenErr Str ]*


## Hardlink the first path to the second path, reusing
## existing streams of their parent directories instead
## of opening new ones.
hardlink : Stream *, Str, Stream *, Str -> Task {} [ HardLinkFailed LinkErr Str ]*
# NOTE: calls `linkat` - https://www.man7.org/linux/man-pages/man2/linkat.2.html
# note that linkat requires dirfd for both fd args, not file fd!
# TODO: AT_SYMLINK_FOLLOW?
# TODO: AT_EMPTY_PATH is how you can make an anonymous tempfile and then give it a name later. Should tempfiles be their own module? I'm starting to think so. Do anonymous tempfiles work on other OSes than Linux?
