import subprocess as sp
from log import log

class Dune:

    command  = "dune"
    comp_exe_name = "ultrac"
    comp_exe_path = "_build/install/default/bin/"

    def __init__(self, filepath):
        self.dune_root = filepath
        log(f"dune path: {self.dune_root}", what="Dune")

    def build(self):
        log("build", what="Dune")
        p = sp.Popen([Dune.command, "build"], cwd=self.dune_root)
        p.wait()

    def clean(self):
        log("clear", what="Dune")
        p = sp.Popen([Dune.command, "clean"], cwd=self.dune_root)
        p.wait()