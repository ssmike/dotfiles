import sys
POST_CODE_EXEC='sys.stdin.close();sys.stdout.close()'
INPUT_FILE, OUTPUT_FILE =['inp', ''], ['outp', '']
FILE_NUMBER= 0 if len(sys.argv) >= 2 and sys.argv[1] == 'LOCAL_RUN' else 1
if INPUT_FILE[FILE_NUMBER] == '': POST_CODE_EXEC=''
else :
    sys.stdin = open(INPUT_FILE[FILE_NUMBER], "r")
    sys.stdout = open(OUTPUT_FILE[FILE_NUMBER], "w")
# -- code beginning


# -- code end

exec(POST_CODE_EXEC);
