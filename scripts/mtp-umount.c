#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

int main() {
    setuid(0);
    unlink("/tmp/android.tmp");
    system("/usr/bin/fusermount -u /Android");
}
