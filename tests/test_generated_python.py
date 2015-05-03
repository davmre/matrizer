import numpy as np
import scipy
import scipy.linalg
import time
import os
import sys



def run_loaded_test(symbolcode, code1, code2, max_seconds=0.1):
    env = {"np": np, "scipy": scipy, "scipy.linalg": scipy.linalg}
    exec(symbolcode, env)

    targets = env['targets']

    env1 = env.copy()
    env2 = env.copy()

    totaltime = 0.0
    time1 = 0.0
    time2 = 0.0
    n = 0
    while totaltime < max_seconds:
        t1 = time.time()
        exec(code1, env1)
        t2 = time.time()
        exec(code2, env2)
        t3 = time.time()
        
        time1 += t2-t1
        time2 += t3-t2
        totaltime += t3-t1
        n += 1

    errs = [np.max(np.abs(np.asarray(env1[target]).flatten()-np.asarray(env2[target]).flatten())) for target in targets]

    relerrs = [np.max(np.abs(np.log(np.asarray(env1[target]).flatten()/np.asarray(env2[target]).flatten()))) for target in targets]
    return targets, errs, relerrs, time1/n, time2/n, env1, env2

def run_test(dirname):
    with open(os.path.join(dirname, "table.py")) as f:
        symbolcode = f.read()

    with open(os.path.join(dirname, "naive.py")) as f:
        naive_code = f.read()

    with open(os.path.join(dirname, "optimized.py")) as f:
        optimized_code = f.read()

    targets, errs, relerrs, naive_time, opt_time, env1, env2 = run_loaded_test(symbolcode, naive_code, optimized_code)
    print "%s: naive %.0fus optimized %.1fus (%.1f%% speedup)." % (dirname, naive_time*1000000, opt_time*1000000, (1 - opt_time/naive_time)*100.0  ),

    if (np.max(errs) > 1e-4 and np.max(relerrs) > 1e-4) or np.isnan(np.max(errs)):
        print  " Errors:"
        for target, err, relerr in zip(targets, errs, relerrs):
            print "   %s: absolute %f relative %f" % (target, err, relerr)

            t1 = np.asarray(env1[target])
            if len(t1.shape) < 2:
                t1 = t1.reshape((1, -1))
            t2 = np.asarray(env2[target])
            if len(t2.shape) < 2:
                t2 = t2.reshape((1, -1))
            np.savetxt(os.path.join(dirname, '%s_naive.txt' % target), t1)
            np.savetxt(os.path.join(dirname, '%s_optimized.txt' % target), t2)
        print "   ... output saved to", dirname
    else:
        print

def run_tests(dirname):
    
    for testdir in os.listdir(dirname):
        run_test(os.path.join(dirname, testdir))

if __name__ == "__main__":
    default_test_dir = "tests/gen_python/"
    run_tests(default_test_dir)
