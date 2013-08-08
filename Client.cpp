//---------------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <iostream>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <curl/curl.h>

using namespace std;

string server;
int port;
std::string g_buf = "";

int get(string object_id);
int put(string s_file);
int del(string object_id);
int init(string host, int port);
int login(string User, string Password);
//---------------------------------------------------------------------------
size_t write_data( char *ptr, size_t size, size_t nmemb, FILE* data)
{
    return fwrite(ptr, size, nmemb, data);
}
static size_t read_callback(void *ptr, size_t size, size_t nmemb, FILE *stream)
{
    size_t retcode;
    curl_off_t nread;

    retcode = fread(ptr, size, nmemb, stream);
    nread = (curl_off_t)retcode;
    fprintf(stderr, "*** We read %" CURL_FORMAT_CURL_OFF_T
          " bytes from file\n", nread);
    return retcode;
}
size_t http_callback(void *buffer, size_t size, size_t count, void *user_p)
{
    g_buf += (char*)buffer;
    return size*count;
}
//---------------------------------------------------------------------------
int main()
{
    init("127.0.0.1", 8080);
    login("UserTest", "PassTest");
    put("/home/pasha/1.jpg");
    get("/v1.0/test/test/1.jpg");
    del("/v1.0/test/test/1.jpg");
    return 0;
}

int get(string object_id)
{
    string url = server + object_id;
    std::string body_filename  = "body";
    FILE *body_file =  fopen(body_filename.c_str(),"w");
    if (body_file == NULL)  return -1;
 
    CURL *curl_handle = curl_easy_init();
    if(curl_handle)
    {
        curl_easy_setopt(curl_handle, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl_handle, CURLOPT_PORT, port);
        curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, body_file);
        curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl_handle, CURLOPT_WRITEHEADER, stdout);

        CURLcode res = curl_easy_perform(curl_handle);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

         curl_easy_cleanup(curl_handle);
    }
    fclose(body_file);
    return 0;
}
int put(string s_file)
{
    FILE * hd_src;
    int hd;
    struct stat file_info;
 
    const char *file = s_file.c_str();
    const char *url = "http://127.0.0.1:8080/v1.0/test/test/1.jpg";

    // Размер файла узнаем из локального файла 
    hd = open(file, O_RDONLY);
    fstat(hd, &file_info);
    close(hd);
    hd_src = fopen(file, "rb");

    CURL *curl_handle = curl_easy_init();
    if(curl_handle) 
    {
        curl_easy_setopt(curl_handle, CURLOPT_URL, url);
        curl_easy_setopt(curl_handle, CURLOPT_READFUNCTION, read_callback);
        curl_easy_setopt(curl_handle, CURLOPT_UPLOAD, 1L);
        curl_easy_setopt(curl_handle, CURLOPT_PUT, 1L);
        curl_easy_setopt(curl_handle, CURLOPT_READDATA, hd_src);
        curl_easy_setopt(curl_handle, CURLOPT_INFILESIZE_LARGE, (curl_off_t)file_info.st_size);

        curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl_handle, CURLOPT_WRITEHEADER, stdout);

        CURLcode res = curl_easy_perform(curl_handle);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        curl_easy_cleanup(curl_handle);
    }
    fclose(hd_src);
    curl_global_cleanup();
    return 0;
}
 
int del(string object_id)
{
    CURL *curl;
    CURLcode res;
    struct stat file_info;
    double speed_upload, total_time;
    string url = server + object_id;
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L);
        curl_easy_setopt(curl,CURLOPT_CUSTOMREQUEST, "DELETE");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, &http_callback);
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_PORT, port);
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }
        else {
        //    std::cout<<"receive data:"<<std::endl<<g_buf<<std::endl;
        }
        curl_easy_cleanup(curl);
    }
    return 0;
}
int login(string User, string Password)
{
    struct curl_slist *headerlist=NULL;
    string name = "X-Auth-User:" + User;
    string key = "X-Auth-Key:" + Password;
    string url = server + "/v1.0";
    CURL *curl_handle = curl_easy_init();
    headerlist = curl_slist_append(headerlist, name.c_str());
    headerlist = curl_slist_append(headerlist, key.c_str());
    if(curl_handle)
    {
        curl_easy_setopt(curl_handle, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl_handle, CURLOPT_PORT, port);
        curl_easy_setopt(curl_handle, CURLOPT_HTTPHEADER, headerlist);
        curl_easy_setopt(curl_handle, CURLOPT_WRITEHEADER, stdout);

        CURLcode res = curl_easy_perform(curl_handle);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

         curl_easy_cleanup(curl_handle);
    }
    return 0;
}
int init(string host, int port)
{
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) 
    {
        curl_easy_setopt(curl, CURLOPT_URL, host.c_str());
        curl_easy_setopt(curl, CURLOPT_PORT, port);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION,&http_callback);
        res = curl_easy_perform(curl);
        switch (res)
        {
            case CURLE_COULDNT_CONNECT:
            case CURLE_COULDNT_RESOLVE_HOST:
            case CURLE_COULDNT_RESOLVE_PROXY: return -1;
            default: 
            {
                server = host;
                ::port = port;
            }
        }

    }
    curl_easy_cleanup(curl);
    return 0;
}
