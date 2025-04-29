#include <bits/stdc++.h>
using namespace std;

string get_module(filesystem::path current_path, string name) {
	using namespace std::filesystem;

	string s;
	s += "module " + name + " = struct\n";

	stringstream ss;
	if (is_directory(current_path)) {
		for (auto const &dir_entry : std::filesystem::directory_iterator{current_path}) {
			path next_path = dir_entry.path();

			string fn = next_path.filename().stem().string();
			if (fn == "dune") continue;
			string module_name = fn;
			module_name[0] = toupper(module_name[0]);

			string t = get_module(next_path, module_name);
			ss << t;
		}
	} else {
		assert(is_regular_file(current_path));
		fstream fs;
		fs.open(current_path);
		ss << string(istreambuf_iterator<char>(fs), istreambuf_iterator<char>());
	}

	string t;
	while (getline(ss, t)) {
		s += "  " + t + "\n";
	}

	s += "end\n";
	return s;
}

string get_all_modules() {
	return get_module("/home/wayne/Documents/OCaml/projects/CP/lib/", "Lib");
}

int main() {
	cin.tie(0);
	ios_base::sync_with_stdio(false);

	string s;

	string target = "(** Definitions of modules here *)";
	while (getline(cin, s)) {
		if (s == target) {
			cout << "(** Template starts*)\n";
			cout << get_all_modules();
			cout << "(** Template ends*)\n";
		} else {
			cout << s << "\n";
		}
	}

	return 0;
}
