#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* tempture() {
    char buffer[50];
    FILE *fp;
    int temp, thermal_throttle;
    char *text = calloc(50, sizeof(char));

    fp = fopen("/sys/devices/platform/coretemp.0/hwmon/hwmon7/temp1_input", "r");
    fgets(buffer, sizeof(buffer), fp);
    temp = atoi(buffer);
    fclose(fp);

    fp = fopen("/sys/devices/system/cpu/cpu0/thermal_throttle/package_throttle_count", "r");
    fgets(buffer, sizeof(buffer), fp);
    thermal_throttle = atoi(buffer);
    fclose(fp);

    sprintf(text, "🌡%2.1f℃ (%d)", temp / 1000.0, thermal_throttle);

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

    sprintf(text, "%s%3.1fGB(%2d%%)", label, used / 1024.0 / 1024, (int) (used / (double) total * 100));

    return text;
}

char* memory() {
    return meminfo("🍫", "MemTotal:", "MemAvailable:");
}

char* swap() {
    return meminfo("🔃", "SwapTotal:", "SwapFree:");
}

typedef struct CPUUtil {
    char *line;
    char *name;
    int active;
    int sum;
    struct CPUUtil *next;
} CPUUtil;

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

int max_percent_cpu_util_per_core(CPUUtil *last, CPUUtil *cur) {
    int max_percent = 0;
    while (last != NULL && cur != NULL) {
        int percent = 100 * (cur->active - last->active) / (cur->sum - last->sum);
        if (max_percent < percent) {
            max_percent = percent;
        }
        last = last->next;
        cur = cur->next;
    }
    return max_percent;
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
    char *text = calloc(50, sizeof(char));

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

    sprintf(text, "❖%3d%%/%3d%%", percent, max_percent_cpu_util_per_core(last, cur));

    free_all_cpu_util(last);
    free_all_cpu_util(cur);

    return text;
}

int main(void) {
    char* _tempture = tempture();
    char* _cpu_util = cpu_util();
    char* _memory = memory();
    char* _swap = swap();

    printf("%s | %s | %s %s\n", _tempture, _cpu_util, _memory, _swap);

    free(_tempture);
    free(_memory);
    free(_swap);
    free(_cpu_util);

    return 0;
}

