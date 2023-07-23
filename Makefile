PROG=tasks
SRCS=tasks.f m_task.f
MAN=

FFLAGS += -fPIC -g
LDFLAGS += -lgfortran -L/usr/local/lib/gcc12/

.include <bsd.prog.mk>

