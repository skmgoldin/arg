import subprocess
import sys

if __name__ == "__main__":
    try:
        target_output = subprocess.check_output(["./model"])
    except:
        print("failed to run model exec")
        quit()
    try:
        actual_output = subprocess.check_output(["./" + sys.argv[1]])
    except:
        print("failed to run target exec")
        quit()
    if target_output == actual_output:
        print('PASS')
    else:
        print('FAIL')
