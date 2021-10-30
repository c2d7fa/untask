# Untask

Untask is a task manager that runs in your terminal. You interact with it by
writing commands in a flexible query language that you can use to make Untask
fit your workflow.

## Documentation

You can run `untask` with a file argument, e.g. `untask tasks.t` to open that
file.

### Syntax

Untask uses a powerful query language that allows you to do complex operations
quite easily. You will use filter expressions to select items, and then use
modify expressions to modify the properties of those items.

#### Filter Expressions

A filter expression is used for selecting items.

The basic component of a filter expression is the basic form
`<property><operator><value>`. This selects all items where `<property>` matches
`<value>` according to the rules of `<operator>`. For example, `depends+8`
selects all items where the `depends` property contains (`+`) the item with ID
`8`, and `urgency:$0` selects all items where the `urgency` property is exactly
0.

If you want two basic forms to match, separate them by a space. For example,
`tags+work urgency>$0` selects all work-related items with a positive urgency.

To match any basic form, separate them by commas. For example, `!date>Today,
urgency>$4` selects all items that have `date` set on or before today or which
have an urgency greater than 4.

You can prefix a basic form with `!` to negate it. Thus, `!status:done` selects
all items which have not been finished.

An empty filter expression selects everything.

There are some special abbreviations that can be used in filter expressions:
- `#tag` for `tags+tag`
- `-#tag` for `tags-tag`
- `/string` or `/{long string}` for `description/string` or `description/{long string}` (and this works with other operators too)
- `{long string}` for `description:{long string}`

#### Modify Expressions

Modify expressions are used to modify existing items or add new items.

The syntax for modify expressions is similar to the syntax for filter
expressions, but the operators have different meanings. Commas cannot be used in
modify expressions.

This example of a modify expression will add item 52 as a dependency and remove
the tag "hobby" if it exists: `depends+52 -#hobby`.

#### Literals

**Strings.** Strings are typed in braces, like this: `{This is a string. There
are many like it, but this one is mine.}`. A string that contains only lowercase
letters can be typed directly, for example `somestring`. This abbreviation is
useful for tags; for example, use `tags-work` to hide items tagged `work`.

**Tasks.** To mention another task, simply use its ID. For example, use `522
copy date+$7` to copy the task with ID `522` and schedule its copy a week from
the original.

**Numbers.** Numbers are typed with a `$` in front. For example, use
`!urgency<$1` to list all items with urgency greater than  or equal to 1.

**Lists.** Lists are typed in square brackets with spaces between the items. For
example, to reset the dependencies of item `29` to just `12` and `20`, type `29
modify depends:[12 20]`.

**Dates.** There are multiple ways to enter a date, and dates may optionally be
associated with a specific time. The following are all valid dates:
`2020-Dec-8`, `2020-12-08`, `2020-Dec-8T13:00`, `2020-12-08T13:00`, `Today`,
`Today+7` (a week from now), `Dec-8`, `Dec-8T13:00`.

#### Contexts

A context is associated with a filter expression and a modify expression. When a
context is active, its associated filter expression and modify expression is
mixed in to all expressions in all commands.

You can create a new context like this:

    > context add project filter #project !status:done !urgency<0 modify #project

Then enable and use the context like so:

    > @project
    @project> #bug

This will list all unfinsihed items tagged "project", "bug" with a non-negative
urgency. You can also enable a context for a single command, like so:

    > @project add {Refactor module}

This will add a task called "Refactor module" that is tagged "project", thanks
to the modify expression set in the `project` context.

Disable contexts like `-@context` and reset all contexts with just `@`. You may
enable and disable multiple contexts at a time.

#### Special Syntax

The syntax `#tag` is identical to `tags+tag` and `-#tag` is identical to
`tags-tag`.

If no property is mentioned, `description` is assumed to be the default. For
example, `/{desc} modify #tag` will add the tag `tag` to all items whose
description contains the text "desc". Likewise, `add {example}` will add a task
with description "example".

In a modify expression, use `property..` to open an editor for the given string
property. Or use simply `..` to open an editor with the item description and its
notes.

### Commands

General:

- `open <tasks.t>` &mdash; Open the given file. The currently open file is
  displayed in the prompt.
- `save` &mdash; Save all changes to the currently open file.
- `exit` &mdash; Exit Untask.

Adding, removing and modifying tasks:

- `add <modify-expression>` &mdash; Create a new item with its properties set
  according to the given modify expression.
- `<filter-expression> modify <modify-expression>` &mdash; Modify all tasks
  matching the given filter expression according to the given modify expression.
- `<filter-expression> copy <modify-expression>` &mdash; Create clones of all
  items matching the given filter expression, and then apply the given modify
  expression to the newly created items.
- `<filter-expression> copy <modify-expression> from <start-date> to <end-date> by <number>` &mdash;
  Like the normal `copy`, but modifies the `date` and
  `wait` properties of the newly created items such that the date of the first
  item is on `<start-date>`, and items are created for each `<number>`th day
  between the start and end dates.
- `<filter-expression> remove` &mdash; Delete all matching items.

Showing tasks:

- `<filter-expression> list` &mdash; List all tasks matching the given filter
  expression.
- `<filter-expression> agenda` &mdash; Display all matching items with the
  `date` property set in an agenda view.
- `<filter-expression> schedule` &mdash; Like `agenda`, but shows a graphical
  schedule view, where `effort` is taken into account.
- `<filter-expression> tree <filter-expression>` &mdash; Show the dependencies
  of all items matched by the left filter-expression, hiding any items that do
  not match the right filter expression.
- `<task-id>` &mdash; Inspect the given task, showing all of its properties,
  including notes.

Managing contexts:

- `@context1 -@context2` &mdash; Enable `context1` and disable `context2`.
- `@` &mdash; Disable all contexts. (This can also be used together with
  `@context`, e.g. `@ @context` to enable *just* `context`.)
- `context list` &mdash; List all contexts.
- `context remove <context>` &mdash; Delete the given context.
- `context add <context> filter <filter-expression> modify <modify-expression>`
  &mdash; Create a new context, and set the associated filter and modify
  expressions.

### Properties

**`status` (`active | inactive | done`):** If set to
inactive, the task cannot be finished; it is considered a blocking dependency.
If set to done, the task is done. It may be useful to have a context like
`context add unfinished filter !status:done`, so you can more clearly see which
items need to be finished.

**`description` (*string*):** The task's title. This is the default property
when no other property is mentioned, so you can easily add a new task with `add
{Description}`. Search with `/{keyword}`.

**`notes` (*string*):** To show a task's notes, simply use the task ID as the
command, e.g. `142` to show notes for `142`. You can edit a task's notes and
it's description with `142 modify ..`.

**`tags` (*list of strings*):** You can use this for whatever you want. For
example, so show items tagged "soon", type `tags+soon list` (or simply `#soon`).

**`urgency` (*number*):** Used for sorting in list view. You can manually set a
task's base urgency. Part of the urgency is also calculated based on any tasks
that are blocked by that task.

**`depends` (*list of tasks*):** A list of tasks that must be finished before
the given task can be finished. When a task has unfinished dependencies, it is
automatically marked as `status:inactive`, no matter what the user-defined
status is.

**`blocks` (*list of tasks*):** The opposite of `depends`. For example, to make
task `1` depend on task `2`, you can do either `1 modify depends+2` or `2 modify
blocks+1`. Note that a task's urgency is calculated based on the urgencies of
tasks it blocks (plus the user-defined base urgency for that task).

**`children` (*list of tasks*):** You can use this property to mention related
tasks. The given tasks are shown as children in the tree view.

**`parents` (*list of tasks*):** Opposite of `children`.

**`date` (*date*):** This is the date on which the given task is displayed in
the agenda view. It doesn't have a strict meaning; you can use it as a deadline,
a soft schedule date, or just a date where you would like to be reminded of the
task.

**`wait` (*date*):** A task is marked as *inactive* until its `wait` date. Use
this to hide tasks that you won't care about until some point in the future.

**`color` (`red | green | yellow | blue | magenta | cyan`):** The task will be
shown as this color. You can use this for whatever you want. It can be useful to
show important tasks in a strong color, like red or yellow.

**`effort` (*number*):** An estimate of how much "effort" the task will take to
complete. This determines the size of the task in the graphical schedule view.

**`order` (*number*):** When two tasks have the same `date`, their order in the
agenda view is determined by this property.

### Operators

Operators have different meanings in filter expressions and modify expressions.
The meaning of an operator may also depend on the type of its left argument.

#### Filter Operators

Any type:

- `property:value` &mdash; Matches if the property has exact value.

Strings:

- `property/{search term}` &mdash; Matches if the property contains `search term`.
- `property<{prefix}` &mdash; Matches if the property starts with `prefix`.
- `property>{suffix}` &mdash; Matches if the property ends with `suffix`.

Lists:

- `property+<value>` &mdash; Matches if the property contains the value.
- `property-<value>` &mdash; Matches if the property does not contain the value.
  (Equivalent to `!property+<value>`.)

Numbers:

- `property>$1` &mdash; Matches if the property is stricly greater than 1. (Use
  `!property<$1` for greater than or equal.)
- `property<$1` &mdash; Matches if the property is stricly less than 1. (Use
  `!property>$1` for less than or equal.)

Date:

- `property>Today` &mdash; Matches if the date is after today.
- `property<Today` &mdash; Matches if the date is before today.

### Modify Operators

Any type:

- `property:<value>` &mdash; Assign the value to the property.

Strings:

- `property<{prefix}` &mdash; Preprend `prefix` to the property.
- `property>{suffix}` &mdash; Append `suffix` to the property.

Numbers:

- `property+$1` &mdash; Add 1 to the property.
- `property-$1` &mdash; Subtract 1 from the property.

Lists:

- `property+<value>` &mdash; Add the value to the list.
- `property-<value>` &mdash; Remove the value from the list if it exists.

Dates:

- `property+$7` &mdash; Add 7 days (a week) to the property.
- `property-$7` &mdash; Subtract 7 days (a week) from the property.

## Building

Untask requires Racket 8.2. On Ubuntu use the PPA `plt/racket` to get the latest
version of Racket:

    sudo apt-add-repository ppa:plt/racket
    sudo apt-get install racket

Then, use `raco` to build and install the application:

    raco pkg install --user --auto --copy

The application will be installed in `~/.local/share/racket/8.2/bin/untask`. Add
this directory to your `PATH` or create a symbolic link to make it available.
