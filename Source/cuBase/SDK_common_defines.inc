// STATIC_CHAINS enables statically allocated Hash chains. If STATIC_CHAINS not defined, allocates
// hash chains dinamically when adding to hash table, allowing to have same item in different hash tables.
// Also hash table rehashes itself without using First and Next pointers when changing hash table size.
{$DEFINE STATIC_CHAINS}
