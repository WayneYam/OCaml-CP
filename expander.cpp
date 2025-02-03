#include <bits/stdc++.h>
using namespace std;

string get_module(filesystem::path p, string name) {
	using namespace std::filesystem;

	string s;
	s += "module " + name + " = struct\n";

	stringstream ss;
	if (is_directory(p)) {
		for (auto const &dir_entry : std::filesystem::directory_iterator{p}) {
			path next_path = dir_entry.path();

			string fn = next_path.filename().stem().string();
			if (fn == "dune") continue;
			string module_name = fn;
			module_name[0] = toupper(module_name[0]);

			string t = get_module(next_path, module_name);
			ss << t;
		}
	} else {
		assert(is_regular_file(p));
		fstream fs;
		fs.open(p);
		ss << string(istreambuf_iterator<char>(fs), istreambuf_iterator<char>());
	}

	string t;
	while (getline(ss, t)) {
		s += "  " + t + "\n";
	}

	s += "end\n";
	return s;
}

string get_all_modules() { return get_module("lib", "Lib"); }

int main() {
	cin.tie(0);
	ios_base::sync_with_stdio(false);

	string s;

	string start = "[@@@ocaml.ppx.context";
	string module_def = "[@@@ocaml.text \" Definition of modules here \"]";
	string end = "[@@@ocaml.text \" End of file \"]";

	bool has_start = false;
	bool has_end = false;
	while (getline(cin, s)) {
		if (s == start) has_start = true;
		if (s == end) has_end = true;

		if (!has_start) continue;
		if (has_end) break;

		if (s == module_def) {
			cout << "(** Template starts*)\n";
			cout << get_all_modules();
			cout << "(** Template ends*)\n";
		} else {
			cout << s << "\n";
		}
	}

	if (!has_start)
		return 1;
	else
		return 0;
}
