#include "Parser.hpp"

int main(int argc, char *argv[])
{
    setlocale(LC_ALL, "spanish");

    if (argc < 2 || argc > 3) {
        std::cout << "usage: ./choco.exe <input_file> [-d]" << std::endl;
        return 1;
    }

    std::string input_file = argv[1];
    bool debug = false;

    if (argc == 3) {
        if (strcmp(argv[2],"-d") != 0) {
            std::cout << "error: " << argv[2] << " invalid"
                << std::endl;
            return 1;
        }
        debug = true;
    }

    Parser P(argv[1], debug);
    if (P.parse())
        std::cout << "\e[0;32mÉxito\e[0m" << std::endl;

    return 0;
}