#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <time.h>

#define CORETEMP_HWMON_ROOT_PATH "/sys/devices/platform/coretemp.0/hwmon"
#define PACKAGE_THROTTLE_COUNT_PATH "/sys/devices/system/cpu/cpu0/thermal_throttle/package_throttle_count"
#define KHZ2GHZ(hz) (hz / 1000. / 1000)
#define SEC2MILLIS(sec) (sec * 1000)
#define NANO2MILLIS(nano) (nano / 1000 / 1000)

#define BLACK "#4E4B42"
#define BRIGHT_BLACK "#635F54"
#define GRAY "#B4AF9A"
#define DARK_WHITE "#CDC8B0"
#define WHITE "#DAD4BB"
#define RED "#CC654C"
#define BLUE "#3BA99F"
#define BRIGHT_BLUE "#42bdb7"
#define BRIGHT_RED "#e06d56"

char *fc = NULL;
void set(char *fg, char *bg) {
    if (fc != NULL) {
        free(fc);
    }

    fc = NULL;

    fc = calloc(256, sizeof(char));
    sprintf(fc, "<fc=%s,%s>", fg, bg);
}

void reset() {
    set(WHITE, BLACK);
}
void emergency() {
    set(WHITE, RED);
}
void ok() {
    set(BRIGHT_BLUE, BLACK);
}

void xmobar_wrap(char *str) {
    if (fc == NULL) {
        return;
    }

    char *tmp = calloc(strlen(str) + 1, sizeof(char));
    strcpy(tmp, str);
    sprintf(str, "%s%s</fc>", fc, tmp);
    free(tmp);
}

int read_int_file(char *root, char *path1, char *path2) {
    char *path = calloc(strlen(root) + strlen(path1) + strlen(path2) + 3, sizeof(char));
    char buffer[256];

    strcpy(path, root);
    if (strlen(path1) > 0) {
        strcat(path, "/");
        strcat(path, path1);
    }
    if (strlen(path2) > 0) {
        strcat(path, "/");
        strcat(path, path2);
    }

    int n = -1;
    FILE *fp = fopen(path, "r");
    if (fp != NULL) {
        fgets(buffer, sizeof(buffer), fp);
        n = atoi(buffer);
        fclose(fp);
    }

    free(path);

    return n;
}

long read_long_file(char *root, char *path1, char *path2) {
    char *path = calloc(strlen(root) + strlen(path1) + strlen(path2) + 3, sizeof(char));
    char buffer[256];

    strcpy(path, root);
    if (strlen(path1) > 0) {
        strcat(path, "/");
        strcat(path, path1);
    }
    if (strlen(path2) > 0) {
        strcat(path, "/");
        strcat(path, path2);
    }

    long n = -1;
    FILE *fp = fopen(path, "r");
    if (fp != NULL) {
        fgets(buffer, sizeof(buffer), fp);
        n = atol(buffer);
        fclose(fp);
    }

    free(path);

    return n;
}


char* tempture() {
    char buffer[50];
    FILE *fp;
    int temp = 0, thermal_throttle;
    char *text = calloc(50, sizeof(char));

    DIR *dir;
    struct dirent *dp;

    dir = opendir(CORETEMP_HWMON_ROOT_PATH);
    if (dir == NULL) {
        return text;
    }
    while ((dp = readdir(dir)) != NULL) {
        if (dp->d_name[0] == '.') {
            continue;
        }

        temp = read_int_file(CORETEMP_HWMON_ROOT_PATH, dp->d_name, "temp1_input");
        if (temp != -1) {
            break;
        }
    }
    closedir(dir);

    fp = fopen(PACKAGE_THROTTLE_COUNT_PATH, "r");
    fgets(buffer, sizeof(buffer), fp);
    thermal_throttle = atoi(buffer);
    fclose(fp);

    double t = temp / 1000.0;
    sprintf(text, "🌡%2.1f℃ (%d)", t, thermal_throttle);
    if (80 < t) {
        emergency();
    } else if (50 >= t) {
        ok();
    }
    xmobar_wrap(text);
    reset();

    return text;
}

typedef struct CPUCoreInfo {
    int core_id;
    int min_freq;
    int min_limit_freq;
    int max_freq;
    int max_limit_freq;
    int cur_freq;
    int base_freq;
    int performance;

    struct CPUCoreInfo *next;
} CPUCoreInfo;

int has_efficient_cores(CPUCoreInfo *p) {
    for (; p != NULL; p = p->next) {
        if (!p->performance) {
            return 1;
        }
    }
    return 0;
}

CPUCoreInfo* find_core_info(CPUCoreInfo *p, int core) {
    for (; p != NULL; p = p->next) {
        if (p->core_id == core) {
            return p;
        }
    }
    return NULL;
}

void free_all_cpu_core_info(CPUCoreInfo *p) {
    if (p != NULL) {
        free_all_cpu_core_info(p->next);
        p->next = NULL;
        free(p);
    }
}


CPUCoreInfo* read_cpu_core_info() {
    char buffer[256];
    FILE *fp;
    int performance_core_min = 0;
    int performance_core_max = 9999;

    fp = fopen("/sys/devices/cpu_core/cpus", "r");
    if (fp != NULL) {
        fgets(buffer, sizeof(buffer), fp);
        fclose(fp);
        int pos = 0;
        while (buffer[pos] != '-') pos++;
        buffer[pos] = '\0';
        performance_core_min = atoi(buffer);
        performance_core_max = atoi(buffer + pos + 1);

    }

    DIR *dir;
    struct dirent *dp;
    CPUCoreInfo *head = NULL;
    CPUCoreInfo *last = NULL;
    dir = opendir("/sys/devices/system/cpu/cpufreq");
    while ((dp = readdir(dir)) != NULL) {
        if (dp->d_name[0] == '.') {
            continue;
        }

        if (strstr(dp->d_name, "policy") != dp->d_name) {
            continue;
        }

        int core_id = atoi(dp->d_name + strlen("policy"));

        if (head == NULL) {
            head = last = calloc(1, sizeof(CPUCoreInfo));
        } else {
            last->next = calloc(1, sizeof(CPUCoreInfo));
            last = last->next;
        }
        last->core_id = core_id;
        last->performance = performance_core_min <= core_id && core_id <= performance_core_max;

        char *path = calloc(256, sizeof(char));
        last->min_freq = read_int_file("/sys/devices/system/cpu/cpufreq", dp->d_name, "cpuinfo_min_freq");
        last->max_freq = read_int_file("/sys/devices/system/cpu/cpufreq", dp->d_name, "cpuinfo_max_freq");
        last->min_limit_freq = read_int_file("/sys/devices/system/cpu/cpufreq", dp->d_name, "scaling_min_freq");
        last->max_limit_freq = read_int_file("/sys/devices/system/cpu/cpufreq", dp->d_name, "scaling_max_freq");
        last->cur_freq = read_int_file("/sys/devices/system/cpu/cpufreq", dp->d_name, "scaling_cur_freq");
        last->base_freq = read_int_file("/sys/devices/system/cpu/cpufreq", dp->d_name, "base_frequency");
    }

    return head;
}

typedef struct CPUUtil {
    char *line;
    char *name;
    int core_id; // -1: package
    int active;
    int sum;
    struct CPUUtil *next;
} CPUUtil;

CPUUtil* find_core_util(CPUUtil *p, int core_id) {
    for (; p != NULL; p = p->next) {
        if (p->core_id == core_id) {
            return p;
        }
    }
    return NULL;
}

CPUUtil* read_cpu_util(FILE* fp) {
    char buffer[256];
    int n;
    int count;

    CPUUtil *cpu_util = calloc(1, sizeof(CPUUtil));
    cpu_util->line = calloc(256, sizeof(char));
    cpu_util->name = calloc(256, sizeof(char));
    CPUUtil *last = cpu_util, *prev = NULL;

    fscanf(fp, "%s", buffer);
    while (1) {
        count = 0;
        if (strncmp("cpu", buffer, 3) == 0) {
            strcpy(last->name, buffer);
            if (strcmp("cpu", buffer) == 0) {
                last->core_id = -1;
            } else {
                last->core_id = atoi(buffer + strlen("cpu"));
            }

            sprintf(last->line + strlen(last->line), " %s", buffer);

            while (fscanf(fp, "%s", buffer) != EOF) {
                if ('0' <= *buffer && *buffer <= '9') {
                    sscanf(buffer, "%d", &n);
                    last->sum += n;
                    if (count++ < 3) {
                        last->active += n;
                    }
                    sprintf(last->line + strlen(last->line), " %s", buffer);
                } else {
                    break;
                }
            }

            last->next = calloc(1, sizeof(CPUUtil));
            last->next->line = calloc(256, sizeof(char));
            last->next->name = calloc(256, sizeof(char));
            prev = last;
            last = last->next;
        } else {
            break;
        }
    }

    free(last->line);
    free(last->name);
    free(last);
    if (prev != NULL) {
        prev->next = NULL;
    }
    return cpu_util;
}

int most_utilized_cpu_core(CPUUtil *last, CPUUtil *cur, CPUCoreInfo *cores, int pcore) {
    int max_percent = 0;
    int max_freq = 0;
    int core_id;
    while (last != NULL && cur != NULL) {
        if (cur->core_id == -1) {
            goto next;
        }

        CPUCoreInfo *core = NULL;
        if (cores != NULL) {
            core = find_core_info(cores, cur->core_id);
            if (core != NULL) {
                if (pcore && !core->performance) {
                    goto next;
                } else if (!pcore && core->performance) {
                    goto next;
                }
            }
        }
        int percent = 100 * (cur->active - last->active) / (cur->sum - last->sum);
        if (max_percent == percent) {
            if (core != NULL && max_freq < core->cur_freq) {
                core_id = cur->core_id;
                max_percent = percent;
                max_freq = core->cur_freq;
            }
        } else if (max_percent < percent) {
            core_id = cur->core_id;
            max_percent = percent;
            if (core != NULL) {
                max_freq = core->cur_freq;
            }
        }

    next:
        last = last->next;
        cur = cur->next;
    }
    return core_id;
}

char* show_cpu_core(int core_id, CPUUtil *last, CPUUtil *cur, CPUCoreInfo *cores) {
    CPUUtil *l = find_core_util(last, core_id);
    CPUUtil *c = find_core_util(cur, core_id);
    int percent = 100 * (c->active - l->active) / (c->sum - l->sum);
    CPUCoreInfo *i  =  find_core_info(cores, core_id);
    char *buffer = calloc(256, sizeof(char));

    if (i == NULL) {
        sprintf(buffer, "%3d%%", percent);
    } else {
        sprintf(buffer, "%3d%%(∿%1.1fGHz|%1.1f〜%1.1fGHz)", percent,
                KHZ2GHZ(i->cur_freq),
                KHZ2GHZ(i->min_limit_freq),
                KHZ2GHZ(i->max_limit_freq));
    }
    return buffer;
}

void free_all_cpu_util(CPUUtil *cpu_util) {
    if (cpu_util != NULL) {
        free_all_cpu_util(cpu_util->next);
        cpu_util->next = NULL;
        free(cpu_util->line);
        free(cpu_util->name);
        free(cpu_util);
    }
}

char* cpu_util() {
    FILE *fp;
    char buffer[256];
    int n;
    CPUUtil *last = NULL;
    CPUUtil *cur;
    int percent = 0;
    char *text = calloc(256, sizeof(char));

    CPUCoreInfo *cores = read_cpu_core_info();
    int has_ecore = has_efficient_cores(cores);

    fp = fopen("/tmp/main_xmobar_cpu_util_last", "r");
    if (fp != NULL) {
        last = read_cpu_util(fp);
        fclose(fp);
    }

    fp = fopen("/proc/stat", "r");
    cur = read_cpu_util(fp);
    fclose(fp);

    fp = fopen("/tmp/main_xmobar_cpu_util_last", "w");
    if (fp != NULL) {
        CPUUtil *iter = cur;
        while (iter != NULL) {
            fprintf(fp, "%s\n", iter->line);
            iter = iter->next;
        }
    }
    fclose(fp);


    if (last == NULL) {
        percent = 0;
    } else {
        percent = 100 * (cur->active - last->active) / (cur->sum - last->sum);
    }

    if (has_ecore) {
        int p_core_id = most_utilized_cpu_core(last, cur, cores, 1);
        char *p_core_str = show_cpu_core(p_core_id, last, cur, cores);
        int e_core_id = most_utilized_cpu_core(last, cur, cores, 0);
        char *e_core_str = show_cpu_core(e_core_id, last, cur, cores);
        sprintf(text, "❖%3d%%/P%s/E%s", percent,
                p_core_str, e_core_str);
        free(p_core_str);
        free(e_core_str);
    } else {
        int p_core_id = most_utilized_cpu_core(last, cur, cores, 1);
        char *p_core_str = show_cpu_core(p_core_id, last, cur, cores);
        sprintf(text, "❖%3d%%/%s", percent, p_core_str);
        free(p_core_str);
    }

    free_all_cpu_core_info(cores);
    free_all_cpu_util(last);
    free_all_cpu_util(cur);

    return text;
}

char* meminfo(char* label,  char* total_label, char* available_label) {
    char buffer[50];
    int num = 0;
    FILE *fp;
    int total, available, used;
    int found = 0;
    char *text = calloc(50, sizeof(char));

    fp = fopen("/proc/meminfo", "r");
    while (found < 2 && fscanf(fp, "%s %d kB\n", buffer, &num) != EOF) {
        if (strcmp(buffer, total_label) == 0) {
            total = num;
            found++;
        } else if (strcmp(buffer, available_label) == 0) {
            available = num;
            found++;
        }
    }
    used = total - available;
    fclose(fp);

    int percent = (int) (used / (double) total * 100);
    sprintf(text, "%s%3.1fGB(%2d%%)", label, used / 1024.0 / 1024, percent);
    if (percent >= 90) {
        emergency();
    }
    xmobar_wrap(text);
    reset();

    return text;
}

char* memory() {
    return meminfo("🍫", "MemTotal:", "MemAvailable:");
}

char* swap() {
    return meminfo("🔃", "SwapTotal:", "SwapFree:");
}

char* copy_getenv_or_defualt(char *varname, char *def) {
    char *value = getenv(varname);
    if (value == NULL) {
        value = def;
    }

    char *buffer = calloc(strlen(value), sizeof(char));
    strcpy(buffer, value);
    return buffer;
}

char* show_network_bps(long bps, char *prefix) {
    char *unit = " bps";
    char *template = "%s%4d%4s";
    if (bps > 1 * 1024 * 1024) { // 1Mbps
        emergency();
    }

    if (bps > 1024) {
        unit = "Kbps";
        bps = bps / 1024;
    }
    if (bps > 1024) {
        unit = "Mbps";
        bps = bps / 1024;
    }
    if (bps > 1024) {
        unit = "Gbps";
        template = "%s%2.1f%4s";
    }

    char *text = calloc(100, sizeof(char));
    if (strcmp(unit, "Gbps") == 0) {
        sprintf(text, "%s%2.1f%4s", prefix, bps / 1024., unit);
    } else {
        sprintf(text, "%s%4ld%4s", prefix, bps, unit);
    }

    xmobar_wrap(text);
    reset();
    return text;
}

long network_segment_retransmit() {
    FILE *fp = fopen("/proc/net/snmp", "r");
    if (fp == NULL) {
        return 0;
    }

    char buffer1[1024];
    char buffer2[1024];

    while (fgets(buffer1, sizeof(buffer1), fp) && fgets(buffer2, sizeof(buffer2), fp)) {
        if (strstr(buffer1, "Tcp:") == buffer1 && strstr(buffer2, "Tcp:") == buffer2) {
            break;
        }
    }

    char *buffer1_lasts = NULL;
    char *buffer1_token;
    char *buffer2_lasts = NULL;
    char *buffer2_token;
    while ((buffer1_token = strtok_r(buffer1_lasts == NULL ? buffer1 : NULL, " ", &buffer1_lasts))
           && (buffer2_token = strtok_r(buffer2_lasts == NULL ? buffer2 : NULL, " ", &buffer2_lasts))) {
        if (strcmp("RetransSegs", buffer1_token) == 0) {
            return atol(buffer2_token);
        }
    }

    return 0;
}

char* network(char *nic) {
    FILE *fp;
    char *text = calloc(256, sizeof(char));
    int n;

    long last_rx_bytes = 0;
    long last_tx_bytes = 0;
    long last_epoch_millis = 0;

    fp = fopen("/tmp/main_xmobar_network_last", "r");
    if (fp != NULL) {
        fscanf(fp, "%ld %ld %ld", &last_rx_bytes, &last_tx_bytes, &last_epoch_millis);
        fclose(fp);
    }

    long cur_rx_bytes = read_long_file("/sys/class/net", nic, "statistics/rx_bytes");
    long cur_tx_bytes = read_long_file("/sys/class/net", nic, "statistics/tx_bytes");
    struct timespec ts;
    timespec_get(&ts, TIME_UTC);
    long epoch_millis = SEC2MILLIS(ts.tv_sec) + NANO2MILLIS(ts.tv_nsec);
    if (cur_rx_bytes < 0 || cur_tx_bytes < 0) {
        strcpy(text, "📶 ");
        strcat(text, nic);
        strcat(text, " NIC is not found");
        emergency();
        xmobar_wrap(text);
        return text;
    }

    fp = fopen("/tmp/main_xmobar_network_last", "w");
    if (fp != NULL) {
        fprintf(fp, "%ld %ld %ld", cur_rx_bytes, cur_tx_bytes, epoch_millis);
    }
    fclose(fp);

    long rx_bps = 0, tx_bps = 0;
    if (last_epoch_millis != 0) {
        rx_bps = (cur_rx_bytes - last_rx_bytes) * 1000 * 8 / (epoch_millis - last_epoch_millis);
        tx_bps = (cur_tx_bytes - last_tx_bytes) * 1000 * 8 / (epoch_millis - last_epoch_millis);
    }

    char *rx_text = show_network_bps(rx_bps, "⬇");
    char *tx_text = show_network_bps(tx_bps, "⬆");

    sprintf(text, "📶%s%s (%ld)", rx_text, tx_text, network_segment_retransmit());

    return text;
}

int main(void) {
    char *nic = copy_getenv_or_defualt("NIC", "unknown");

    reset();
    /* printf("tempture\n"); */
    char* _tempture = tempture();
    /* printf("cpu_util\n"); */
    char* _cpu_util = cpu_util();
    /* printf("memory\n"); */
    char* _memory = memory();
    /* printf("swap\n"); */
    char* _swap = swap();
    char* _network = network(nic);

    printf("%s | %s | %s %s | %s\n", _tempture, _cpu_util, _memory, _swap, _network);

    free(_tempture);
    free(_memory);
    free(_swap);
    free(_cpu_util);
    free(_network);

    free(nic);

    return 0;
}

