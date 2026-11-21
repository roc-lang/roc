"""Run argv[2:], write wall/user/sys/peak-RSS of the child to argv[1] as JSON, exit with its status."""
import json
import resource
import subprocess
import sys
import time

out = sys.argv[1]
start = time.perf_counter()
proc = subprocess.run(sys.argv[2:])
wall = time.perf_counter() - start
usage = resource.getrusage(resource.RUSAGE_CHILDREN)
with open(out, "w") as f:
    json.dump(dict(rc=proc.returncode, wall=wall, user=usage.ru_utime, sys=usage.ru_stime, max_rss_kb=usage.ru_maxrss), f)
sys.exit(proc.returncode)
