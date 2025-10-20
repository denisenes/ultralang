import subprocess as sp

class Dune:

    command  = "dune"
    comp_exe_name = "ultrac"
    comp_exe_path = "_build/install/default/bin/"

    def __init__(self, filepath):
        self.dune_root = filepath
        print(f"[Dune] dune path: {self.dune_root}")

    def build(self):
        print("[Dune] build")
        p = sp.Popen([Dune.command, "build"], cwd=self.dune_root)
        p.wait()

    def clean(self):
        print("[Dune] clear")
        p = sp.Popen([Dune.command, "clean"], cwd=self.dune_root)
        p.wait()