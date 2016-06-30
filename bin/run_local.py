#!/usr/local/bin/python3

import argparse
import os
import subprocess
from configparser import ConfigParser


class LocalDockerRunner(object):

    def __init__(self, env_file, image, section="DEFAULT", memory=256):
        self.validate_args(env_file, image, memory)

        self.__init_config(env_file)
        self.section = section
        self.image = image
        self.memory = memory

    def __init_config(self, env_file):
        self.config = ConfigParser()
        self.config.read(env_file)

    def start(self):
        docker_cmd = self.__build_docker_command()

        self.__run_cmd(docker_cmd)

    def __build_docker_command(self):
        docker_cmd = ["docker", "run", "-d"]

        # Handle port
        port = self.config.get(self.section, "port")
        if port:
            docker_cmd.extend(["-p", "{port}:{port}".format(port=port)])

        # Handle memory
        if self.memory:
            docker_cmd.extend(["-m", "{}m".format(self.memory)])

        # Handle environment variables
        for item in self.config._defaults.items():
            docker_cmd.extend(["-e", "\"{}={}\"".format(item[0].upper(), item[1])])

        # Handle image location
        docker_cmd.append(self.image)

        # Convert the array into a single string
        docker_cmd = ' '.join(docker_cmd)
        return docker_cmd

    def __run_cmd(self, docker_cmd):
        print(docker_cmd)
        proc = subprocess.Popen(docker_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
        stdout, stderr = proc.communicate()
        if stderr:
            print("ERROR: {}".format(stderr))
            print("stdout: {}".format(stdout))
        else:
            container_id = stdout.decode("utf-8").strip()
            print("Container started with container ID: {}".format(container_id))

    def validate_args(self, env_file, image, memory):
        if not os.path.exists(env_file):
            raise ValueError("{} does not exist.".format(env_file))
        if memory and not isinstance(memory, int):
            raise ValueError("memory must be an integer.")


def parse_input():
    parser = argparse.ArgumentParser()
    parser.add_argument("--env-file", dest="env_file", required=True,
                        help="the .env file with environment values to run with")
    parser.add_argument("--image", dest="image", required=True, help="The image you want to run.")
    args = parser.parse_args()
    return args


if __name__ == "__main__":
    args = parse_input()
    runner = LocalDockerRunner(args.env_file, args.image)
    runner.start()
