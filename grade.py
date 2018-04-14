import os
import json
import subprocess
import shutil
import re

shutil.copy2("test.ml", "/autograder/submission/")
shutil.move("/autograder/submission/input/", "/autograder/submission/stu-input")
shutil.copytree("input", "/autograder/submission/input/")
os.chdir("/autograder/submission/")
os.mkdir("output")
with open("_tags", "w"):
    pass

# Tests
out_build, err_build = subprocess.Popen(" ".join(["make", "test"]),
                                        stdout=subprocess.PIPE,
                                        stdin=subprocess.PIPE,
                                        stderr=subprocess.PIPE,
                                        shell=True).communicate("")
out_run, err_run = subprocess.Popen(" ".join(["./test",
                                              "-output-file",
                                              "results.log"]),
                                    stdout=subprocess.PIPE,
                                    stdin=subprocess.PIPE,
                                    stderr=subprocess.PIPE,
                                    shell=True).communicate("")

test_cases = 0
tests_passed = 0
tests_failed = 0

# Check if test passed
if os.path.exists("results.log"):
    with open("results.log") as f:
        lines = f.readlines()
        test_cases = int(re.search('Cases: (\d+).', lines[-7]).group(1))
        tests_failed = int(re.search('Failures: (\d+).', lines[-4]).group(1))
        tests_passed = test_cases - tests_failed

total_score = {
    'output': "",
    'tests': [
        {
            "name":
                "Test cases: %d/%d tests passed" % (tests_passed, test_cases),
            "output": out_build + "\n" + out_run,
            "score": tests_passed * 2
        },
        {
            "name": "stderr",
            "output": err_build + "\n" + err_run
        },
    ]
}

if(os.path.isdir("/autograder/results")):
    resultsjson = open("/autograder/results/results.json","w")
    resultsjson.write(json.dumps(total_score))
    resultsjson.close()
else:
    print("local test")
    print json.dumps(total_score, indent=4, sort_keys=True)
