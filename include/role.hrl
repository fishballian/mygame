-ifndef(ROLE_HRL).
-define(ROLE_HRL, true).

-record(r_role_base, {name :: binary(),
                      passwd :: binary(),
                      sex :: pos_integer(),
                      lv :: pos_integer(),
                      category :: pos_integer(),
                      gold :: non_neg_integer()}).

-record(r_role_attr, {max_hp :: non_neg_integer(),
                      hp :: non_neg_integer(),
                      lucky :: non_neg_integer(),
                      attack :: non_neg_integer(),
                      defence :: non_neg_integer()}).
                      
-endif.
