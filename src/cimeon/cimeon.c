#include <inttypes.h>
#include "raw_parse.c"

YYACTIONTYPE state_count = YYNSTATE;
YYACTIONTYPE rule_count = YYNRULE;

extern int can_reduce_and_shift(yyParser *pParser, YYCODETYPE token) {
    return 0;
}

extern YYACTIONTYPE current_state(yyParser *pParser) {
    return pParser->yystack[pParser->yyidx].stateno;
}

extern node top_custom_state_ptr(yyParser *pParser) {
    node result;
    memcpy(&result, &pParser->yystack[0].minor, sizeof(node));
    return result;
}