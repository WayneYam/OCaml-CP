#include <bits/stdc++.h>

using namespace std;

#ifdef zisk
void __print(int x) { cerr << x; }
void __print(long x) { cerr << x; }
void __print(long long x) { cerr << x; }
void __print(unsigned x) { cerr << x; }
void __print(unsigned long x) { cerr << x; }
void __print(unsigned long long x) { cerr << x; }
void __print(float x) { cerr << x; }
void __print(double x) { cerr << x; }
void __print(long double x) { cerr << x; }
void __print(char x) { cerr << '\'' << x << '\''; }
void __print(const char *x) { cerr << '\"' << x << '\"'; }
void __print(const string &x) { cerr << '\"' << x << '\"'; }
void __print(bool x) { cerr << (x ? "true" : "false"); }

template <typename T, typename V>
void __print(const pair<T, V> &x) {
	cerr << '{';
	__print(x.first);
	cerr << ',';
	__print(x.second);
	cerr << '}';
}
template <typename T>
void __print(const T &x) {
	int f = 0;
	cerr << '{';
	for (auto &i : x) cerr << (f++ ? "," : ""), __print(i);
	cerr << "}";
}
void _print() { cerr << "]\n"; }
template <typename T, typename... V>
void _print(T t, V... v) {
	__print(t);
	if (sizeof...(v)) cerr << ", ";
	_print(v...);
}
#define debug(x...) cerr << "[" << #x << "] = [", _print(x)
template <class T>
void pary(T l, T r) {
	while (l != r) cout << *l << " ", l++;
	cout << endl;
}
#else
#define debug(x...) (void)0
template <class T>
void pary(T l, T r) {}
#endif

#ifdef ONLINE_JUDGE
#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx2,bmi,bmi2,lzcnt,popcnt")
#endif

mt19937 rng(chrono::steady_clock::now().time_since_epoch().count());
#define vi vector<int>
#define pb push_back
#define eb emplace_back
#define mp make_pair
#define mt make_tuple
#define pii pair<int, int>
#define pll pair<ll, ll>
#define F(n) Fi(i, n)
#define Fi(i, n) Fl(i, 0, n)
#define Fl(i, l, n) for (int i = l; i < n; i++)
#define RF(n) RFi(i, n)
#define RFi(i, n) RFl(i, 0, n)
#define RFl(i, l, n) for (int i = n - 1; i >= l; i--)
#define all(v) begin(v), end(v)
#define siz(v) ((long long)(v.size()))
#define get_pos(v, x) (lower_bound(all(v), x) - begin(v))
#define sort_uni(v) sort(begin(v), end(v)), v.erase(unique(begin(v), end(v)), end(v))
#define mem(v, x) memset(v, x, sizeof v)
#define ff first
#define ss second
#define RAN(a, b) uniform_int_distribution<int>(a, b)(rng)	// inclusive
#define cmax(a, b) (a = max(a, b))
#define cmin(a, b) (a = min(a, b))
typedef long long ll;
typedef long double ld;

/* TEMPLATE STARTS HERE */

/* TEMPLATE ENDS HERE */

signed main() {
	cin.tie(0);
	ios_base::sync_with_stdio(false);

	return 0;
}
