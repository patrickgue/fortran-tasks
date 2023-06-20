PROG=tasks
SRCS=tasks.f m_task.f
MAN=

FFLAGS += -fPIC
LDFLAGS += -lgfortran -L/usr/local/lib/gcc12/

.include <bsd.prog.mk>

