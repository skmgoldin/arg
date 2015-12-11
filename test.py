import subprocess
import sys
import os

TESTS = []
for f in os.listdir("tests"):
	if ".arg" in f:
		TESTS.append(f[:-4])

#supp = " >/dev/null 2>&1";
supp = " > argerr"

if __name__ == "__main__":

		for i in range(0, len(TESTS)):
			program = TESTS[i];

			try:
					err = os.system("./argc tests/"+program+supp)
					if err != 0:
						print(str(i+1) + ". "+program +": FAIL")
						print("\tFailed to compile")
						print "\t"+open("argerr", "r").read()
						continue;
					os.system("gcc -w -o "+program+" tests/"+program+".c")
					arg_output = subprocess.check_output(["./"+program])
					os.system("rm " + program)
					os.system("rm tests/"+program+".c")
			except:
					quit()
			try:
					os.system("gcc -w -o model tests/"+program+"-model.c")
					model_output = subprocess.check_output(["./model"])
					os.system("rm model")
			except:
					print("failed to run arg exec")
					quit()
			if arg_output == model_output:
					print(str(i+1) + ". "+program +": PASS")
			else:
					print(str(i+1) + ". "+program +": FAIL")
					arg_lines = arg_output.split("\n")
					model_lines = model_output.split("\n")
					for i in range(0,len(arg_lines)):
						if arg_lines[i] != model_lines[i]:
							print "\t"+str(i+1) + ". arg: " + arg_lines[i] + "\t model: " + model_lines[i]
