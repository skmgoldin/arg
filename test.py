import subprocess
import sys
import os

TESTS = ["helloworld", "count","function"]
       #  "calculations",
        # "count"];

supp = " >/dev/null 2>&1";

if __name__ == "__main__":

		for i in range(0, len(TESTS)):
			program = TESTS[i];

			try:
					err = os.system("./arg tests/"+program+supp)
					if err != 0:
						print(str(i+1) + ". "+program +": FAIL")
						print("\tFailed to compile")
						continue;
					os.system("gcc -o "+program+" tests/"+program+".c")
					target_output = subprocess.check_output(["./"+program])
					os.system("rm " + program)
					os.system("rm tests/"+program+".c")
			except:
					quit()
			try:
					os.system("gcc -o model tests/"+program+"-model.c")
					actual_output = subprocess.check_output(["./model"])
					os.system("rm model")
			except:
					print("failed to run target exec")
					quit()
			if target_output == actual_output:
					print(str(i+1) + ". "+program +": PASS")
			else:
					print(str(i+1) + ". "+program +": FAIL")
