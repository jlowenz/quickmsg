#include "clock_sync.hpp"
#include <iostream>

int
main(int argc, char** argv)
{
  if (argc < 2) {
    std::cout << "Usage: clk_sync_client <hostname>" << std::endl;
    return -1;
  }
  SyncClient cli(argv[1]);
  cli.run();
  return 0;
}
