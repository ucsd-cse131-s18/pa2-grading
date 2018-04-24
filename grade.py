import os
import json
import subprocess
import shutil
import re

# change this when running the final autograder
runFullTests = False

if runFullTests:
    shutil.copy2("fullTests.ml", "/autograder/submission/")
    shutil.move("/autograder/submission/fullTests.ml", "/autograder/submission/test.ml")
else:
    shutil.copy2("test.ml", "/autograder/submission/")

shutil.move("/autograder/submission/input/", "/autograder/submission/stu-input")
shutil.copytree("input", "/autograder/submission/input/")
os.chdir("/autograder/submission/")

if not os.path.exists("output"):
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
tests_errored = 0
tests_failed = 0

# Check if test passed
if os.path.exists("results.log"):
    with open("results.log") as f:
        lines = f.readlines()
        test_cases = int(re.search('Cases: (\d+).', lines[-7]).group(1))
        tests_failed = int(re.search('Failures: (\d+).', lines[-4]).group(1))
        tests_errored = int(re.search('Errors: (\d+).', lines[-5]).group(1))
        tests_passed = test_cases - tests_failed - tests_errored

topLevelOutput = ""
if not runFullTests:
    topLevelOutput = "The test suite below contains 79 tests. After the final turnin deadline for this PA, we will rerun your submission against a test suite that contains 110 tests. Your final score will be out of 110."

total_score = {
    'output': topLevelOutput,
    'tests': [
        {
            "name":
                "Test cases: %d/%d tests passed" % (tests_passed, test_cases),
            "output": out_build + "\n" + out_run,
            "score": tests_passed
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
