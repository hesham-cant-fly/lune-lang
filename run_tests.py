#!/usr/bin/env python3
# inspired by 'https://github.com/icxd/amulet/blob/master/run_tests.py'

import os
import sys
import subprocess
from typing import List

COMPILER_PATH = "./zig-out/bin/lune-lang"
TESTS_DIR = "unitests"
VERBOSE = False

for f in sys.argv[1:]:
    if f == "--verbose":
        VERBOSE = True

def run_test(test_file: str) -> bool:
    compiler_result = subprocess.run(
        [COMPILER_PATH, test_file], capture_output=True, text=True,
    )
    if compiler_result.returncode != 0:
        print(f"\033[31;1merror: \033[0mfailed to compile {test_file}")
        if VERBOSE:
            print(compiler_result.stderr)
        return False
    return True

def main():
    # Gets all necessary files that are in 'TESTS_DIR'
    test_files = [
        os.path.join(root, file)
        for root, _, files in os.walk(TESTS_DIR)
        for file in files
        if file.endswith(".lune") # Check if the file is a Lune file
        if not file.startswith("ignore-") # ignores some files
    ]

    passing_tests: List[str] = []
    failing_tests: List[str] = []

    for test_file in test_files:
        passed = run_test(test_file)
        if passed:
            print(f"\033[32;1mTest {test_file} passed.\033[0m")
            passing_tests.append(test_file)
        else:
            print(f"\033[31;1mTest {test_file} failed.\033[0m")
            failing_tests.append(test_file)

    print("\nSummary: ")
    print(f"   \033[32;1m{len(passing_tests)} tests passed.\033[0m")
    print(f"   \033[31;1m{len(failing_tests)} tests failed.\033[0m")

if __name__ == "__main__":
    main()
