set -e -u

./build.sh
./run.sh -t assembly testing.dcf -o testing.s --opt="algsimp,cse,cp,dce"
gcc -no-pie -O0 testing.s -o testing
./testing
