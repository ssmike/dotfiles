#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

int main() {
    if (access("/tmp/android.tmp", F_OK ) != -1) return 0;
    setuid(0);
    system("/sbin/modprobe fuse");
    system("/usr/bin/go-mtpfs  -allow-other=true /Android &");
    fclose(fopen("/tmp/android.tmp", "w"));
}
