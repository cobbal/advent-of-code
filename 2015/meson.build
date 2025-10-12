project(
    '2015', 'c',
    version : '1.0.0',
    default_options : ['warning_level=3', 'c_std=c23']
)

_2015 = executable(
    '_2015',
    'day-01/day-01.c',
    'day-02/day-02.c',
    'day-03/day-03.c',
    'day-04/day-04.c',
    'day-05/day-05.c',
    'common.c',
    'set.c',
    'md5.c',
    'main.c',
    install : false
)

test('test', _2015)