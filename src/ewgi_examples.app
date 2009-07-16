{application, ewgi_examples,
 [{description, "ewgi_examples"},
  {vsn, "0.01"},
  {modules, [
    ewgi_examples,
    ewgi_examples_app,
    ewgi_examples_sup,
    ewgi_examples_web,
    ewgi_examples_deps
  ]},
  {registered, []},
  {mod, {ewgi_examples_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
