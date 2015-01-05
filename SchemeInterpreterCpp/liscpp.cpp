
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <tuple>

class Func
{


private:
	
	std::string var;
	std::string exp;

	std::vector<Func> internals;
	
};

class SchemeInterpreter
{

public:
	void tokenize(std::string s) {
		int i = 0, j = 0;
		for (j = 0; j < s.size(); j++)
		{
			if (s[j] == '(' || s[j] == ' ' || s[j] == ')') {
				if (i != j) tokens.push_back(s.substr(i, j - i));
				if (s[j] == '(') tokens.push_back("(");
				else if (s[j] == ')') tokens.push_back(")");
				i = j + 1;
			}
		}
	}

	void parser() {
		if (tokens[0] == "(") {
			
		}
	}

private:
	std::vector<std::string> tokens;
	std::vector<std::string> func;
};

int main(void) {

	std::tuple<int, int, int> t = std::make_tuple(1,2,3);
	
	std::cout << std::get<0>(t) << std::endl;
	std::cout << std::get<1>(t) << std::endl;
	std::cout << std::get<2>(t) << std::endl;

	SchemeInterpreter si;

	si.tokenize("(define x (list (list 1 2) (list 3 4)))");
	si.tokenize("(define my-tree (list 1 (list 2 (list 3 4) (list 5 6)) (list 7 (list 8))))");

	return 0;
}