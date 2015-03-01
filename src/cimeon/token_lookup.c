struct viable_token {
    unsigned char token; //YYCODETYPE
    unsigned char action; //YYACTIONTYPE
};
struct viable_token_list {
    unsigned char count; //YYACTIONTYPE
    struct viable_token* actions;
};

static struct viable_token unchecked_actions[18] = {
    {1, 9},
    {1, 9},
    {5, 4},
    {9, 12},
    {10, 5},
    {6, 6},
    {8, 3},
    {9, 11},
    {7, 13},
    {4, 14},
    {3, 2},
    {2, 8},
    {1, 9},
    {4, 14},
    {4, 14},
    {4, 14},
    {1, 9},
    {1, 9},
};
struct viable_token_list unchecked_actions_list[16] = {
    {1, &unchecked_actions[0]},
    {1, &unchecked_actions[1]},
    {1, &unchecked_actions[2]},
    {2, &unchecked_actions[3]},
    {2, &unchecked_actions[5]},
    {1, &unchecked_actions[7]},
    {1, &unchecked_actions[8]},
    {1, &unchecked_actions[9]},
    {1, &unchecked_actions[10]},
    {1, &unchecked_actions[11]},
    {1, &unchecked_actions[12]},
    {1, &unchecked_actions[13]},
    {1, &unchecked_actions[14]},
    {1, &unchecked_actions[15]},
    {1, &unchecked_actions[16]},
    {1, &unchecked_actions[17]},
};
static struct viable_token checked_actions[0] = {
};
struct viable_token_list checked_actions_list[16] = {
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
    {0, 0},
};