# brain_diseases_wp3_app

Shiny app for brain disease work package 3 project

Uses version v6_2025-03-19 results from project


TODO:

1) Right now, we have a "parent" closest relative, where we look for "parent" closest
   relative for a control, but DO NOT require the parents to be of the same sex. This causes
   some irregularities/hidden problems that could simply be solved by adding the very obvious
   requirement, that "parent" closest relatives also has to be the same sex. If this is done
   we can remove the relative_relation variable, and simply group closest_relative_type in
   the analyses if we want to make a "parent" category?
2) Cost plots seems totally of, and does not really correspond with focus in the cost
   analyses tables. Remove for now, and rethink how we want to summarize the cost results
   visually. Maybe only make plot for "any disorder"? Restucture plot around that focus?
3) For plots: it makes things really difficult that the 0-24 agegroup does not have a "pooled"
   closest_relative_type corresponding to the other agegroups. Would it make sense to add this
   to the analyses?
