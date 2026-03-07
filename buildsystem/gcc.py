import subprocess as sp
from log import log
from glob import glob

class GCC:

    cpp_comp = "g++"
    args: list[str] = []


    def set_arg(self, arg: str):
        self.args.append(arg)


    def build_dir(self, dir: str, out: str, shared: bool = False):
        log(f"build sources from: {dir}", what="GCC")

        self.set_arg(f"-I{dir}")
        files = glob(f"{dir}/*.cpp")
        
        args = [GCC.cpp_comp] + self.args + files + ["-o", out] + (["-shared"] if shared else [])
        p = sp.Popen(args, cwd=dir)
        code = p.wait()

        log(f"compiler finished with error code: {code}")