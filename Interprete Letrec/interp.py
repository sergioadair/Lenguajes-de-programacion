from typing import NewType


class Binding:
    def __init__(self, name, value):
        self.name = name
        self.value = value

Environment = NewType('Environment', list)
empty_env = Environment([])

def extend_env(name, value, env):
    env.insert(0, Binding(name, value))
    return env

def extend_env_rec(pname, bvar, pbody, saved_env):
    placeholder = 0
    new_env = extend_env(pname, placeholder, saved_env)
    new_env[0].value = procedure(bvar, pbody, new_env)
    return new_env

def procedure(var, body, env):
    return lambda val: value_of(body, extend_env(var, val, env))

def apply_procedure(proc, val):
    return proc(val)

def is_number(val):
    if type(val) in (float, int):
        return True
    else:
        return False


def interp(e: tuple):
    v = None
    try:
        v = value_of(e, empty_env)
    except:
        print('An error occurred while interpreting. Please check your code.')
    print('INTERPRETER RESULT: ')
    return v

def value_of(e: tuple, env: Environment):
    if e: 
        etype = e[0]

        match etype:
            case 'const-exp':
                return e[1]
            case 'diff-exp':
                v = [value_of(e[1], env), value_of(e[2], env)]
                if is_number(v[0]) and is_number(v[1]):
                    return v[0] - v[1]
                for i in v:
                    if not is_number(i): print('Value is not a number: %s' % i)
                return
            case 'zero?-exp':
                v = value_of(e[1], env)
                if is_number(v):
                    return True if v == 0 else False
                print('Value is not a number: %s' % v)
                return
            case 'if-exp':
                v = value_of(e[1], env)
                if isinstance(v, bool):
                    return value_of(e[2], env) if v else value_of(e[3], env)
                print('Value is not bool: %s' % v)
                return
            case 'var-exp':
                var = e[1]
                for bind in env:
                    if var == bind.name: return bind.value
                print('Undefined variable: %s' % var)
                return
            case 'let-exp':
                return value_of(e[3], extend_env(e[1], value_of(e[2], env), env))
            case 'proc-exp':
                return procedure(e[1], e[2], env)
            case 'call-exp':
                return apply_procedure(value_of(e[1], env), value_of(e[2], env))
            case 'letrec-exp':
                return value_of(e[4], extend_env_rec(e[1], e[2], e[3], env))
        print('It\'s not an expression: %s' % etype)
    return