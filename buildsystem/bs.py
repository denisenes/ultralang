import os
from os.path import join
from dune import Dune
from file import *


class BContext:
    def __init__(self, run_dir):
        self.run_dir    : str = run_dir
        self.build_dir  : str = run_dir + "/build"
        self.bin_dir    : str = self.build_dir + "/bin"
        self.ultrac_dir : str = run_dir + "/compiler"

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
    d = Dune(ctx.ultrac_dir)

    d.clean()
    d.build()

    exe = join(d.dune_root, Dune.comp_exe_path, Dune.comp_exe_name)
    copy(exe, join(ctx.bin_dir, Dune.comp_exe_name))


if __name__ == '__main__':
    ctx = BContext(os.getcwd())
    print(ctx)

    init_build_dir(ctx)
    build_ultrac(ctx)
