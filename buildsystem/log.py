import datetime

def log(msg: str, what=None):
    now = datetime.datetime.now().time()
    prefix = f"[{now}] [{what}]" if what else "" 
    print(f"{prefix} {msg}")

def logStage(msg: str):
    log("-" * 30)
    log(f"\t {msg}")
    log("-" * 30)