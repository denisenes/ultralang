find . \( -name "*.cpp" -o -name "*.h" \) -exec clang-format -i -style=file {} \;
find . \( -name "*.cpp" -o -name "*.h" \) -exec clang-tidy -fix {} -- -x c++ -std=c++17 \;
find . -name "*.ml" -exec ocamlformat --inplace {} \;


