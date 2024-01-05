import os
import subprocess
import csv
import functools

results = []

def compile_ultra(testname):
    return os.system('./main.exe compile ' + testname)

def compile_gcc():
    return os.system('cd runtime && make compile && mv ultra ../ultra')

def process_comp_err(name):
    print("=======================")
    print('Name:     ' + name)
    print("Result: compilation error")
    print("=======================")
    results.append([name, False])

def test(name, expected):
    global results

    test_out = subprocess.check_output(['./ultra']).decode('utf-8').rstrip()
    print("=======================")
    print('Name:     ' + name)
    print('Result:   ' + test_out)
    print('Expected: ' + expected)
    print("=======================")
    results.append([name, test_out == expected])
    os.system('rm ./ultra')
    os.system('cd runtime && rm ultra && rm ultra.s')

def runtest(name, expected):
    res1 = compile_ultra(name)
    res2 = compile_gcc()
    if res1 == 0 and res2 == 0:
        test(name, expected)
    else:
        process_comp_err(name)

def main():
    data = []
    with open('tests.list.csv', newline='') as tests_file:
        rdr = csv.reader(tests_file, delimiter=',', quoting=csv.QUOTE_NONE, skipinitialspace=True)
        for row in rdr:
            data.append([row[0], row[1]])
    
    print(data)
    for t, e in data:
        runtest(t, e)

    print(results)
    print("VVVVVVVVVVVVVVVVVVVVVV")
    names = [row[0] for row in data]
    for n, res in results:
        pretty_res = lambda b : 'OK' if b else 'FALSE'
        print("Test " + n + ": " + pretty_res(res))
    print("VVVVVVVVVVVVVVVVVVVVVV")
    print('Total:  ' + str(len(names)))
    print('Passed: ' + str(functools.reduce(lambda acc, x : acc+1 if x[1] else acc, results, 0)))
    print('Failed: ' + str(functools.reduce(lambda acc, x : acc+1 if not x[1] else acc, results, 0)))


if __name__ == "__main__":
    main()