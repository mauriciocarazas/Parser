#ifndef TOKEN_HPP
#define TOKEN_HPP
using namespace std;
#include <string>

struct Token
{
    std::string pos;
    std::string lex;

    Token();
    Token(std::string _pos, std::string _lex);
    void set(std::string _pos, std::string _lex);
};

#endif // TOKEN_HPP