#include <bits/stdc++.h>

using namespace std;
signed main() {
	cin.tie(0);
	ios_base::sync_with_stdio(false);
	char c;
	deque<char> buffer;
	auto write_buffer = [&]() {
		while (!buffer.empty()) {
			cout << buffer.front();
			buffer.pop_front();
		}
	};

	string target = "[@";
	auto test_annotate = [&]() {
		for (int i = 0; i < (int)target.size(); i++) {
			if ((int)buffer.size() > i && buffer[i] != target[i]) return false;
		}
		return true;
	};

	int state = 0;	// 0 -> normal, 1 -> locaked
	int current_level = 0;
	int locked_level = 0;

	while (cin.get(c)) {
		if (c == '[') current_level++;
		if (c == ']') current_level--;

		if (state == 0) {
			buffer.push_back(c);
			if (!test_annotate()) {
				write_buffer();
			} else if (buffer.size() == target.size()) {
				state = 1;
				locked_level = current_level - 1;
				buffer.clear();
			}
		} else if (state == 1) {
			if (current_level == locked_level) state = 0;
		}
	}

	return 0;
}
