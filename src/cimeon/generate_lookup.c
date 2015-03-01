#include <stdlib.h>
#include "raw_parse.c"

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

struct viable_token {
    YYCODETYPE token;
    YYACTIONTYPE action;
};

struct shift_state_list {
    size_t count;
    struct viable_token actions[YYSTACKDEPTH];
};

struct reduce_state_list {
    size_t count;
    YYACTIONTYPE actions[YYSTACKDEPTH];
};

void add_shift_entry(struct shift_state_list* shift_states,
                     struct viable_token pair) {
    for (size_t i = 0; i < shift_states->count; i++) {
        if (shift_states->actions[i].token == pair.token
            && shift_states->actions[i].action == pair.action)
            return;
    }
    shift_states->actions[shift_states->count] = pair;
    shift_states->count++;
}

void expand_reduce_state(struct shift_state_list* shift_states,
                         struct reduce_state_list* reduce_states,
                         struct shift_state_list* new_states,
                         YYACTIONTYPE state,
                         YYACTIONTYPE action) {
    for (size_t i = 0; i < shift_states[action].count; i++) {
        add_shift_entry(new_states, shift_states[action].actions[i]);
    }
    for (size_t i = 0; i < reduce_states[action].count; i++) {
        if (action != reduce_states[action].actions[i])
            expand_reduce_state(shift_states, reduce_states, new_states, state, reduce_states[action].actions[i]);
    }
}

void print_lookup_table(char* name_prefix, struct shift_state_list* states) {
    size_t actions_count = 0;
    for (int i = 0; i < YYNSTATE; i++) {
        actions_count += states[i].count;
    }
    printf("static struct viable_token %s_actions[%d] = {\n", name_prefix, actions_count);
    for(size_t i = 0; i < YYNSTATE; i++) {
        for(size_t j = 0; j < states[i].count; j++) {
            printf("    {%d, %d},\n", states[i].actions[j].token, states[i].actions[j].action);
        }
    }    
    printf("};\n");
    printf("struct viable_token_list %s_actions_list[%d] = {\n", name_prefix, YYNSTATE);
    size_t count = 0;
    for(size_t i = 0; i < YYNSTATE; i++) {
        printf("    {%d, ", states[i].count);
        if (states[i].count > 0) {
            printf("&unchecked_actions[%d]", count);
            count += states[i].count;
        }
        else {
            printf("0");
        }
        printf("},\n");
    }
    printf("};");
}

void print_lookup_tables(struct shift_state_list* shift_states,
                         struct shift_state_list* reduce_states) {
    printf("struct viable_token {\n    %s token; //YYCODETYPE\n    %s action; //YYACTIONTYPE\n};\n", TOSTRING(YYCODETYPE), TOSTRING(YYACTIONTYPE));
    printf("struct viable_token_list {\n    %s count; //YYACTIONTYPE\n    struct viable_token* actions;\n};\n\n", TOSTRING(YYACTIONTYPE));
    size_t max_shift = 0;
    for (int i = YYNSTATE - 1; i >= 0; i--) {
        if (shift_states[i].count > 0) {
            max_shift = i + 1;
            break;
        }
    }
    size_t shift_count = 0;
    for(size_t i = 0; i < max_shift; i++) {
        shift_count += shift_states[i].count;
    }
    
    print_lookup_table("unchecked", shift_states);
    printf("\n");
    print_lookup_table("checked", reduce_states);
}

int main() {
    struct shift_state_list shift_states[YYNSTATE];
    struct reduce_state_list reduce_states[YYNSTATE];
    struct shift_state_list expanded_reduce_states[YYNSTATE];

    for (int i = 0; i < YYNSTATE; i++) {
        shift_states[i].count = 0;
        reduce_states[i].count = 0;
        expanded_reduce_states[i].count = 0;
    }

    unsigned char max_reduce_count = 0;
    for (size_t i = 0; i < sizeof(yyRuleInfo) / sizeof(yyRuleInfo[0]); i++) {
        if (yyRuleInfo[i].nrhs > max_reduce_count) {
            max_reduce_count = yyRuleInfo[i].nrhs;
        }
    }

    yyParser parser;
    for (int stateno = 0; stateno < YYNSTATE; stateno++) {
        for (int token = 1; token <= TOKEN_COUNT; token++) {
            parser.yyidx = max_reduce_count;
            parser.yystack[max_reduce_count].stateno = stateno;
            YYACTIONTYPE  yyact = yy_find_shift_action(&parser, token);
            if (yyact < YYNSTATE) {
                struct shift_state_list* state_ptr = &shift_states[stateno];
                state_ptr->actions[state_ptr->count].token = token;
                state_ptr->actions[state_ptr->count].action = yyact;
                state_ptr->count++;
            }
            else if (yyact < YYNSTATE + YYNRULE) {
                for (int parent_state = 0; parent_state < YY_REDUCE_MAX; parent_state++) {
                    YYCODETYPE yygoto = yyRuleInfo[yyact - YYNSTATE].lhs;
                    int i = yy_reduce_ofst[parent_state] + yygoto;
                    if (yy_lookahead[i] != yygoto)
                        continue;
                    struct reduce_state_list* state_ptr = &reduce_states[stateno];
                    for(size_t j = 0; j < state_ptr->count; j++) {
                        if(state_ptr->actions[j] == yy_action[i])
                            goto next_token;
                    }
                    state_ptr->actions[state_ptr->count] = yy_action[i];
                    state_ptr->count++;
                }
            }
            next_token:;
        }
    }
    for (int i = 0; i < YYNSTATE; i++) {
        for (size_t j = 0; j < reduce_states[i].count; j++) {
            expand_reduce_state(shift_states, reduce_states, &expanded_reduce_states[i], i, reduce_states[i].actions[j]);
        }
    }
    // If reduce rule can follow only a single token then
    // there's no point checking parent states
    for (int i = 0; i < YYNSTATE; i++) {
        if(expanded_reduce_states[i].count == 1) {
            add_shift_entry(&shift_states[i], expanded_reduce_states[i].actions[0]);
            expanded_reduce_states[i].count = 0;
        }
    }
    print_lookup_tables(shift_states, expanded_reduce_states);
}