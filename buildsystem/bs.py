import os
from log import log, logStage
from os.path import join
from dune import Dune
from file import *
from contextlib import chdir
import subprocess as sp
import glob

class BContext:
    def __init__(self, run_dir):
        self.run_dir    : str = run_dir
        self.build_dir  : str = run_dir + "/build"
        self.bin_dir    : str = self.build_dir + "/bin"
        self.ultrac_dir : str = run_dir + "/compiler"
        self.test_dir   : str = run_dir + "/test"

    def __str__(self):
        return f"""Context:
        Run dir:   {self.run_dir}
        Build dir: {self.build_dir}
        """


def init_build_dir(ctx: BContext):
    bdir = ctx.build_dir
    rmdir_ex(bdir)
    os.mkdir(bdir)
    os.mkdir(ctx.bin_dir)


def build_ultrac(ctx: BContext):
    logStage("Build compiler")    

    d = Dune(ctx.ultrac_dir)

    d.clean()
    d.build()

    source = join(d.dune_root, Dune.comp_exe_path, Dune.comp_exe_name)
    target = join(ctx.bin_dir, Dune.comp_exe_name) 
    copy(source, target)
    chmod(target, 766)


def test(ctx: BContext):
    logStage("Test compiler")

    test_dir = ctx.test_dir
    with chdir(test_dir):
        tests = glob.glob(f"{test_dir}/*.ul")
        for t in tests:
            log(f"executing test {t}", what="Test")
            ultrac = os.path.join(ctx.bin_dir, "ultrac")
            p = sp.Popen([ultrac, "-c", t])
            try:
                p.wait(timeout=3)
            except sp.TimeoutExpired:
                p.kill()
                log(f"Test {t} is killed by timeout", what="Error")

if __name__ == '__main__':
    ctx = BContext(os.getcwd())
    log(ctx)

    init_build_dir(ctx)
    build_ultrac(ctx)
    test(ctx)