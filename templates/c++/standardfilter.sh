#!/bin/zsh
export TASKNAME=main

alias intLL="sed -e '/#define INF.*/d' \
                     | sed -e '/using namespace std/a\typedef int MYINTEGERTYPE\;\n#define int long long\n#define INF (1LL<<62)\n' \
					 | sed -e 's/int main/MYINTEGERTYPE main/g;s/%d/%I64d/g'"
cat ./$TASKNAME.cpp |  sed '/.*freopen(\"inp\", \"r\", stdin).*/d' \
			   |  sed '/.*freopen(\"outp\", \"w\", stdout).*/d' \
			   |  sed 's/.*\/\/.*freopen/freopen/g' \
			   |  cat > /tmp/source
