#include <pthread.h>
int main (int argc, char **argv){
    pthread_mutex_t mut;
    pthread_mutex_init(&mut, 0);
    return 0; // all well
}
